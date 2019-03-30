#!/usr/bin/env python3
import yaml
import sys
from yaml import Loader
import json


def generate_elements(elements, attributes):
    elems = []
    for (name, definition) in sorted(elements.items()):
        elem = {'name': name, 'attributes': []}
        for attr in definition['attributes']:
            if isinstance(attr, str):
                elem['attributes'].append(normalize_attribute(get_attribute(attributes, attr)))
            else:
                elem['attributes'].append(normalize_attribute(attr))
        for attr in find_attributes(attributes, name):
            elem['attributes'].append(normalize_attribute(get_attribute(attributes, attr)))
        elems.append(elem)
    return elems


def normalize_attribute(attr):
    return {
        'name': attr['name'],
        'type': attribute_type_as_list(attr['type']),
        'default':  attr['default'] if 'default' in attr else None
    }


def eval_attributes(attributes, groups):
    new_attributes = {}
    for name, attribute in attributes.items():
        new_attribute = {
            'type': attribute['type'],
            'default': attribute['default'] if 'default' in attribute else None
        }
        if 'elements' in attribute:
            new_elements = []
            for element in attribute['elements']:
                if element.startswith('@'):
                    group = element[1:]
                    if group in groups:
                        for element2 in groups[group]:
                            new_elements.append(element2)
                    else:
                        raise Exception('Group %s not found' % group)
                else:
                    new_elements.append(element)
            new_attribute['elements'] = sorted(new_elements)
        else:
            new_attribute['elements'] = []
        new_attributes[name] = new_attribute
    return new_attributes


def get_attribute(attributes, name):
    if name in attributes:
        return {
            'name': name,
            'type': attributes[name]['type'],
            'default': attributes[name]['default'] if 'default' in attributes[name] else None
        }
    return None


def find_attributes(attributes, element_name):
    attrs = []
    for name, definition in sorted(attributes.items()):
        if 'elements' in definition and element_name in definition['elements']:
            attrs.append(name)
    return attrs


def attribute_type_as_list(t):
    if isinstance(t, str):
        return [t]
    else:
        return t


def generate_attributes(attributes, specific_attribs):
    attrs = {}
    for attribute_name, attribute in attributes.items():
        new_attribute = {}
        if len(attribute['elements']) > 0:
            for element in attribute['elements']:
                new_attribute[element] = {'type': attribute['type'], 'default': attribute['default']}
            attrs[attribute_name] = new_attribute

    for attribute_name, attribute in specific_attribs.items():
        if attribute_name not in attrs:
            attrs[attribute_name] = {}
        for element_name in attribute:
            attrs[attribute_name][element_name] = attribute[element_name]

    attrs2 = []
    for attribute_name, attribute_elements in sorted(attrs.items()):
        elements = []
        for element_name, elem in sorted(attribute_elements.items()):
            elements.append({'element': element_name, 'type': elem['type'], 'default': elem['default']})
        attrs2.append({'name': attribute_name, 'elements': elements})
    return attrs2


def specific_attributes(elements, common_attributes):
    attributes = {}
    for element_name, element in elements.items():
        for attribute in element['attributes']:

            if isinstance(attribute, dict):
                if attribute['name'] not in attributes:
                    attributes[attribute['name']] = {}
                attributes[attribute['name']][element_name] = {
                    'type': attribute_type_as_list(attribute['type']),
                    'default': attribute['default'] if 'default' in attribute else None
                }
            else:
                if attribute not in attributes:
                    attributes[attribute] = {}
                attributes[attribute][element_name] = {
                    'type': common_attributes[attribute]['type'],
                    'default': common_attributes[attribute]['default']}
    return attributes


def main():

    if len(sys.argv) > 1:
        s = open(sys.argv[1]).read()
    else:
        s = sys.stdin.read()
    spec = yaml.load(s, Loader=Loader)

    groups = spec['element-groups']
    groups['all'] = [elem for elem in sorted(spec['elements'])]
    common_attrs = eval_attributes(spec['attributes'], groups)
    elements = generate_elements(spec['elements'], common_attrs)
    specific_attrs = specific_attributes(spec['elements'], common_attrs)
    attributes = generate_attributes(common_attrs, specific_attrs)

    print(json.dumps({
        'elements': elements,
        'attributes': attributes
    }))


if __name__ == '__main__':
    main()
