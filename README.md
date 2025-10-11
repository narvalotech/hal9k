# Hal9k - Home automation scripts

This repo contains the scripts I used in the past for home automation.

Ran on SBCL on aarch64. See the `ccl` branch for running on armhf (32-bit).

## Features

- Light control
- Temperature control
- Lightweight automation (e.g. light timer)
- Simple web interface

## System description

- Zigbee devices connected to zigbee2mqtt
- Lisp scripts talk and consume MQTT messages directly
- `home.lisp`: thermostat, physical switch mapping & automation
- `web.lisp`: web interface
- `push.lisp`: upload temperatures to grafana
