#!/usr/bin/env python3
import unittest
import template_utils


class TemplateUtilsTest(unittest.TestCase):

    def test_camelCase(self):
        self.assertEqual('Auto', template_utils.camel_case('auto'))
        self.assertEqual('RemoveFreeze', template_utils.camel_case('remove_freeze'))

    def test_size(self):
        self.assertEqual(0, template_utils.size([]))
        self.assertEqual(0, template_utils.size(['']))
        self.assertEqual(5, template_utils.size(['one', 'two', 'three']))


if __name__ == '__main__':
    unittest.main()

