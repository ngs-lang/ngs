#!/usr/bin/env python
import os
import sys

import boto.ec2

import tabular.tabular as tabular

FIELDS = 'id', 'ip_address', 'private_ip_address', 'groups', 'state', 'key_name', 'instance_type', 'launch_time', 'image_id', 'root_device_type', 'root_device_name'

def reservations2instances(reservations):
    instances = []
    for r in reservations:
        instances += r.instances
    return instances


w = tabular.Writer(*FIELDS)

conn = boto.ec2.connect_to_region('us-east-1', aws_access_key_id=os.getenv('AMAZON_ACCESS_KEY_ID'), aws_secret_access_key=os.getenv('AMAZON_SECRET_ACCESS_KEY'))
reservations = conn.get_all_instances()
instances = reservations2instances(reservations)

for i in instances:
    line = {f: getattr(i, f) for f in FIELDS}
    line['groups'] = map(lambda g: g.id, line['groups'])
    w.write(**line)
w.close()

