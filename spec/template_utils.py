def camel_case(s):
    return "".join(t.capitalize() for t in s.split("_"))


def size(arr):
    if len(arr) == 0:
        return 0
    return max([len(s) for s in arr])
