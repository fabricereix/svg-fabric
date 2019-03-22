#!/usr/bin/env python3
import unittest
import spec2json

ELEMENTS = {
  'svg': {
    'attributes': ['width']
  },
  'rect': {
    'attributes': ['x', 'width']
  },
  'circle': {
    'attributes': ['r']
  }
}

ELEMENT_GROUPS = {
    'graphics': ['rect', 'circle']
}

ATTRIBUTES = {
  'fill': { 'type': ['paint'], 'default': 'black', 'elements': ['rect']},
  'id': {'type': ['string'], 'elements': ['@all']},
  'opacity': {'type': ['opacity'], 'elements': ['@graphics']},
  'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
  'r': {'type': ['length', 'percentage'], 'default': 0},
  'x': {'type': ['length', 'percentage'], 'default': 0}
}


def generate_elements(elements, element_groups, attributes):
    elems = []
    for (name, definition) in sorted(elements.items()):
        elem = {'name': name, 'attributes': []}
        for attr in definition['attributes']:
            if isinstance(attr, str):
               elem['attributes'].append(get_attribute(attributes, attr))
        for attr in find_attributes(attributes, element_groups, name):
            elem['attributes'].append(get_attribute(attributes, attr))
        elems.append(elem)
    return elems


def get_attribute(attributes, name):
  if name in attributes:
      attr = {'name': name, 'type': attributes[name]['type']}
      if 'default' in attributes[name]:
          attr['default'] = attributes[name]['default']
      return attr
  return None


def find_attributes(attributes, element_groups, element_name):
    attrs = []
    for name, definition in sorted(attributes.items()):
        if 'elements' in definition:
            if element_name in definition['elements']:
               attrs.append(name)
            for element in definition['elements']:
                if element.startswith('@'):
                    if element[1:] in element_groups and element_name in element_groups[element[1:]]:
                        attrs.append(name)
    return attrs


class SpecToJsonTest(unittest.TestCase):

    def test_get_attribute(self):
        self.assertEqual({'name': 'x', 'type': ['length', 'percentage'], 'default': 0},
                get_attribute(ATTRIBUTES, 'x'))

    def test_generate_elements(self):
        #print(generate_elements(ELEMENTS, ATTRIBUTES))
        ELEMENT_GROUPS['all'] = [elem for elem in ELEMENTS]
        self.assertEqual([
            {
              'name': 'circle',
              'attributes': [
                  {'name': 'r', 'type':['length', 'percentage'], 'default':0},
                  {'name': 'id', 'type':['string']},
                  {'name': 'opacity', 'type':['opacity']}
              ]
            },
            {
              'name': 'rect',
              'attributes': [
                  {'name': 'x', 'type':['length', 'percentage'], 'default':0},
                  {'name': 'width', 'type':['auto', 'length', 'percentage'], 'default':'auto'},
                  {'name': 'fill', 'type':['paint'], 'default':'black'},
                  {'name': 'id', 'type':['string']},
                  {'name': 'opacity', 'type':['opacity']}
              ]
            },
            {
              'name': 'svg',
              'attributes': [
                  {'name': 'width', 'type':['auto', 'length', 'percentage'], 'default':'auto'},
                  {'name': 'id', 'type':['string']}
              ]
            },
        ],generate_elements(ELEMENTS, ELEMENT_GROUPS, ATTRIBUTES))


    def test_find_attribute(self):
        self.assertEqual(['fill', 'opacity'], find_attributes(ATTRIBUTES,ELEMENT_GROUPS, 'rect'))

if __name__ == '__main__':
    unittest.main()
