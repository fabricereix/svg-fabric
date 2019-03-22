#!/usr/bin/env python3
import unittest
import spec2json

ELEMENTS = {
  'svg': {
    'attributes': ['width', 'height']
  },
  'rect': {
    'attributes': ['x', 'y', 'width', 'height']
  },
  'circle': {
    'attributes': ['r']
  }
}

ELEMENT_GROUP = {
        'graphics': ['rect', 'circle']
}

ATTRIBUTES = {
  'fill': { 'type': 'paint', 'elements': ['rect']},
  'height': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
  'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
  'r': {'type': ['length', 'percentage'], 'default': 0},
  'x': {'type': ['length', 'percentage'], 'default': 0},
  'y': {'type': ['length', 'percentage'], 'default': 0}
}


def generate_elements(elements, attributes):
    elems = []
    for (name, definition) in elements.items():
        elem = {'name': name, 'attributes': []}
        for attr in definition['attributes']:
            if isinstance(attr, str):
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


class SpecToJsonTest(unittest.TestCase):

    def test_get_attribute(self):
        self.assertEqual({'name': 'x', 'type': ['length', 'percentage'], 'default': 0},
                get_attribute(ATTRIBUTES, 'x'))

    def test_generate_elements(self):
        self.assertEqual([
            {
              'name': 'circle',
              'attributes': [
                  {'name': 'r', 'type':['length', 'percentage'], 'default':0}
              ]
            },
            {
              'name': 'rect',
              'attributes': [
                  {'name': 'x', 'type':['length', 'percentage'], 'default':0},
                  {'name': 'y', 'type':['length', 'percentage'], 'default':0},
                  {'name': 'width', 'type':['auto', 'length', 'percentage'], 'default':'auto'},
                  {'name': 'height', 'type':['auto', 'length', 'percentage'], 'default':'auto'}
              ]
            },
            {
              'name': 'svg',
              'attributes': [
                  {'name': 'width', 'type':['auto', 'length', 'percentage'], 'default':'auto'},
                  {'name': 'height', 'type':['auto', 'length', 'percentage'], 'default':'auto'}
              ]
            },
        ],generate_elements(ELEMENTS, ATTRIBUTES))


if __name__ == '__main__':
    unittest.main()
