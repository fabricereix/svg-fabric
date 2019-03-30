def camel_case(s):
    return "".join(t.capitalize() for t in s.split("_"))

