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


conn = boto.ec2.connect_to_region('us-east-1', aws_access_key_id=os.getenv('AMAZON_ACCESS_KEY_ID'), aws_secret_access_key=os.getenv('AMAZON_SECRET_ACCESS_KEY'))
reservations = conn.get_all_instances()
instances = reservations2instances(reservations)

tabular.write_objects_list(instances, columns=FIELDS)
# tabular.write_objects_list(instances, exclude_class_fields={
#     'boto.ec2.instance.Instance': ['connection'],
#     'boto.ec2.blockdevicemapping.BlockDeviceType': ['connection'],
# })
