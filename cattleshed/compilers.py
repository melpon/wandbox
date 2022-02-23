import json

def merge(*args):
    result = {}
    for dic in args:
        result.update(dic)
    return result

class Switches(object):
    def resolve_conflicts(self, pairs):
        conflicts = [p[0] for p in pairs]
        return [(p[0], merge(p[1], { "conflicts": conflicts })) for p in pairs]

    def make_default(self):
        return {
            "mono-optimize":{
                "flags":"-optimize",
                "display-name":"Optimization",
            },
        }

    def make(self):
        return merge(
            self.make_default())

class Compilers(object):
    def resolve_format(self, formats, **kwargs):
        def typedo(value):
            if isinstance(value, str):
                return value.format(**kwargs)
            elif isinstance(value, (list, dict)):
                return self.resolve_format(value, **kwargs)
            else:
                return value

        if isinstance(formats, list):
            return [typedo(value) for value in formats]
        else:
            return {key:typedo(value) for key,value in formats.items()}

    def make_common(self, names, formats):
        params = {
            name[0]: self.resolve_format(formats, name=name[0], **name[1]["params"])
            for name in names
        }
        after = {
            name[0]: name[1]["after"]
            for name in names
        }
        compilers = [merge(
            { "name": name[0] },
            params[name[0]],
            after[name[0]],
        ) for name in names]

        return compilers

    def make_mono(self):
        NAMES = [
            ("mcs-2.6.7", {
                "params": {
                    "mono": "mono-2.6.7",
                },
                "after": {
                    "display-name":"gmcs for Unity on iOS",
                    "display-compile-command":"gmcs -out:prog.exe prog.cs",
                    "compile-command":["/usr/local/mono-2.6.7/bin/gmcs-custom", "-out:prog.exe", "prog.cs"],
                    "run-command":["/usr/local/mono-2.6.7/bin/mono-custom", "prog.exe"]
                },
            }),
        ]
        FORMATS = {
            "displayable":True,
            "language":"C#",
            "output-file":"prog.cs",
            "compiler-option-raw":True,
            "compile-command":["/usr/local/{mono}/bin/mcs", "-out:prog.exe", "prog.cs"],
            "version-command":["/bin/sh", "-c", "/usr/local/{mono}/bin/mcs --version | head -1 | cut -d' ' -f5"],
            "swithes":["mono-optimize"],
            "initial-checked":[],
            "display-name":"mcs",
            "display-compile-command":"mcs -out:prog.exe prog.cs",
            "run-command":["/usr/local/{mono}/bin/mono", "prog.exe"]
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make(self):
        return (
            self.make_mono()
        )

def make_config():
    return {
        "switches": Switches().make(),
        "compilers": Compilers().make(),
        "templates": {},
    }

if __name__ == '__main__':
    print(json.dumps(make_config(), indent=4))
