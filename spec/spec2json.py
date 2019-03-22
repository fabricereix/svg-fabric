#!/usr/bin/env python3
import yaml
import sys
from yaml import Loader
import json


def gen_attr(d, name):
    d2 = {'type': d['type']}
    d2['name'] = name
    if 'default' in d:
        d2['default'] = d['default']
    return d2


def gen_element(name, elem, attributes):
    elem_attributes = []
    for attribute in elem['attributes']:
        if isinstance(attribute, str):
            definition = attributes[attribute]
            elem_attribute = {
               'name': attribute,
               'type': definition['type']
            }
            if 'default' in definition:
                elem_attribute['default'] = definition['default']
            elem_attributes.append(elem_attribute)
        else:
            elem_attribute = {
                'name': attribute['name'],
                'type': attribute['type']
            }
            if 'default' in attribute:
                elem_attribute['default'] = attribute['default']
        elem_attributes.append(elem_attribute)
    return {'name': name, 'attributes': elem_attributes}


def main():
    s = sys.stdin.read()
    spec = yaml.load(s, Loader=Loader)
    #print(spec)
    elements = [gen_element(name, elem, spec['attributes']) for (name, elem) in sorted(spec['elements'].items())]
    attributes = []


    groups = spec['element-groups']
    groups['all'] = [elem for elem in spec['elements']]
    attrs = {}
    for (name, attr) in spec['attributes'].items():
        if 'elements' in attr:
            attrs[name] = {'elements':[]}
            for elem in attr['elements']:
                if elem.startswith('_'):
                    for elem2 in spec['element-groups'][elem[1:]]:
                        attrs[name]['elements'].append(gen_attr(attr, elem2))
                else:
                    attr_data = {'name': elem, 'type': attr['type']}
                    if 'default' in attr:
                        attr_data['default'] = attr['default']
                    attrs[name]['elements'].append(attr_data)
    for (key, attr) in sorted(attrs.items()):
        attributes.append({
            'name': key,
            'elements': attr['elements']
        })
    print(json.dumps({'elements': elements, 'attributes': attributes}))


if __name__ == '__main__':
    main()
