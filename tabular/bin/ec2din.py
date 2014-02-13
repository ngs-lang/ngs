#!/usr/bin/env python
import os
import sys

import boto.ec2

import tabular.tabular as tabular

FIELDS = 'id', 'ip_address', 'private_ip_address', 'state', 'launch_time', 'groups', 'key_name', 'instance_type', 'image_id'

def reservations2instances(reservations):
    instances = []
    for r in reservations:
        instances += r.instances
    return instances


conn = boto.ec2.connect_to_region(
    os.getenv('AWS_DEFAULT_REGION', 'us-east-1'),
    aws_access_key_id=os.getenv('AMAZON_ACCESS_KEY_ID'),
    aws_secret_access_key=os.getenv('AMAZON_SECRET_ACCESS_KEY')
)
reservations = conn.get_all_instances()
instances = reservations2instances(reservations)

# TODO: improve. Very weak.
def sanitize_tag_name(tag_name):
    return 'tag_' + tag_name.lower().replace('-', '_')

def line_tags_transformator(line):
    for k, v in line['tags'].items():
        line[sanitize_tag_name(k)] = v
    for k in list(set(all_tags_names)-set(line['tags'].keys())):
        line[k] = None

all_tags_names = list({sanitize_tag_name(k) for instance in instances for k in instance.tags.keys()})

tabular.write_objects_list(
    instances,
    display_columns=FIELDS,
    columns_rules=[
        tabular.ColumnRuleExcludeByName('connection'),
    ],

    lines_transformators = [
        line_tags_transformator
    ]
)
