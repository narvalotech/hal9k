#!/usr/bin/env python3
#
import paho.mqtt.client as mqtt
import time
import datetime
import json


th = {'default': 19,
      'rachel': 22,
      'chambre': 22,
      'bureau': 20}
th_delta = 0.5
active_bureau = [4, 18]
br_minmax = [20, 255]
enable_bureau = False
force_bureau = False
timeouts = []


def add_timeout(seconds, fn):
    endtime = time.monotonic() + seconds

    for t in timeouts:
        if t['fn'] == fn:
            t['time'] = endtime
            return

    timeouts.append({'time': endtime, 'fn': fn})

def turn_off_hallway():
    topic = f'z2m/light-entree'
    set_state(topic, False)

def is_inside_time(start, end, now=None):
    if now is None:
        now = datetime.datetime.now()

    start_time = now.replace(hour=start, minute=start)
    end_time = now.replace(hour=end, minute=end)

    if start_time > end_time:
        midnight = now.replace(hour=0, minute=0, second=0)

        # between start and midnight
        if now < midnight and now > start_time:
            return True
        # between midnight and end
        if now > midnight and now < end_time:
            return True

        return False

    if now > start_time and now < end_time:
        return True
    else:
        return False


def set_brightness(topic, value):
    print(f'Set {topic} brightness to {value}')
    client.publish(topic + '/set/brightness', value)


def set_state(topic, state):
    # print(f'Set {topic} to {state}')

    if state:
       client.publish(topic + '/set/state', 'ON')
    else:
       client.publish(topic + '/set/state', 'OFF')

def handle_heater(payload, temp_topic):
    try:
        temp = json.loads(payload)['temperature']
    except:
        print(f'Unexpected payload format {payload}')
        return

    name = temp_topic.replace('z2m/temp-', '')
    topic = f'z2m/prise-{name}'
    try:
        threshold = th[name]
    except:
        threshold = th['default']
        print(f'Using default temp {threshold}')

    clock = time.strftime('%H:%M:%S')
    print(f'[{clock}] [{name}]: temp {temp}')

    if 'bureau' in topic:
        active = active_bureau
        if not force_bureau and not is_inside_time(active[0], active[1], now=None):
            # print(f'Outside working hours, heater [{topic}] OFF')
            set_state(topic, 0)
            return

        print(f'Enable: {enable_bureau}')
        if not enable_bureau and not force_bureau:
            set_state(topic, 0)
            return

    if temp > threshold + th_delta:
        print(f'[{name}]: temp = {temp} > {threshold} heater [{topic}] OFF')
        set_state(topic, 0)
    elif temp < threshold - th_delta:
        print(f'[{name}]: temp = {temp} < {threshold} heater [{topic}] ON')
        set_state(topic, 1)
    else:
        print(f'[{clock}] [{name}]: temp {temp}')

def unexpected_format(payload, topic):
        print(f'Unexpected payload {topic}: {payload}')

def handle_switch(payload, topic):
    try:
        action = json.loads(payload)['action']

        if 'brightness' in action and 'chambre' in topic:
            topic = 'z2m/light-chambre'

            if '_up' in action:
                set_brightness(topic, br_minmax[1])
            elif '_down' in action:
                set_brightness(topic, br_minmax[0])
            else:
                return

        elif ('on' in action or 'off' in action):
            state = 'on' in action

            if 'cuisine' in topic:
                for light in ['cuisine', 'entree', 'couloir', 'manger']:
                    topic = f'z2m/light-{light}'
                    set_state(topic, state)
            else:
                name = topic.replace('z2m/switch-', '')
                topic = f'z2m/light-{name}'
                set_state(topic, state)

    except:
        unexpected_format(payload, topic)


def handle_door(payload, topic):
    try:
        door_closed = json.loads(payload)['contact']
    except:
        unexpected_format(payload, topic)
        return

    # Only enable light when door is opened
    if door_closed:
        return

    topic = f'z2m/light-entree'
    set_state(topic, True)

    add_timeout(60 * 5, turn_off_hallway)

def publish_therm(name):
    topic = f'z2m/therm-{name}'
    client.publish(topic, th[name])
    print(f'thermostat {name} = {th[name]}')

def handle_thermostat(payload, topic):
    # Types of messages:
    # /therm-chambre/set (29) -> set temp to 29, publish to /therm-chambre
    # /therm-chambre/get -> publish temp to /therm-chambre
    if '/set' not in topic and '/get' not in topic:
        return

    if '/get' in topic:
        name = topic.replace('z2m/therm-', '').replace('/get', '')
        publish_therm(name)
        return

    # '/set' in topic
    try:
        temp = float(payload)
        name = topic.replace('z2m/therm-', '').replace('/set', '')
        th[name] = temp
        publish_therm(name)
    except:
        unexpected_format(payload, topic)
        return

def handle_force(payload, topic):
    """Enables/disables a heater"""
    try:
        global force_bureau
        force_bureau = 'on' in payload.decode()
        set_state('z2m/prise-bureau', force_bureau)
        print(f'Force bureau: {force_bureau}')
    except:
        unexpected_format(payload, topic)
        return

def handle_enable(payload, topic):
    """Enables/disables a heater"""
    try:
        global enable_bureau
        enable_bureau = 'on' in payload.decode()
        print(f'Enable bureau: {enable_bureau}')
    except:
        unexpected_format(payload, topic)
        return


def on_message(client, userdata, msg):
    if 'force-' in msg.topic:
        handle_force(msg.payload, msg.topic)

    if 'enable-' in msg.topic:
        handle_enable(msg.payload, msg.topic)

    if 'therm-' in msg.topic:
        handle_thermostat(msg.payload, msg.topic)

    if 'temp-' in msg.topic:
        handle_heater(msg.payload, msg.topic)

    if 'switch-' in msg.topic:
        handle_switch(msg.payload, msg.topic)

    if 'door-' in msg.topic:
        handle_door(msg.payload, msg.topic)

def on_connect(client, userdata, flags, rc):
    print("Connected with result code "+str(rc))
    client.subscribe("z2m/#")

client = mqtt.Client()
client.on_connect = on_connect
client.on_message = on_message

client.connect("192.168.10.175", 1883, 60)

client.loop_start()

print('hello')

while True:
    # TODO: handle disconnects
    # TODO: check for timeouts
    time.sleep(1)

    t2 = []
    for t in timeouts:
        # execute callback when timeout has expired
        if time.monotonic() > t['time'] and t['fn']:
            t['fn']()
        else:
            t2.append(t)

    timeouts = t2
