# Wandbox

Wandbox is a social compilation service. The service is here: https://wandbox.org/

You can also access via API: https://github.com/melpon/wandbox/blob/master/kennel2/API.rst

These programs are licensed by Boost Software License 1.0.

## Wandbox on Your Editor

You can use Wandbox on your editor.

Vim: https://github.com/rhysd/wandbox-vim

Emacs: https://github.com/kosh04/emacs-wandbox

xyzzy: https://gist.github.com/kikairoya/7544234

## Wandbox as a Background Infrastructure

You can use Wandbox API from another services (that is, Wandbox API's HTTP response header has `Access-Control-Allow-Origin: *`).

Following services are using Wandbox on background.

### jBatch

jBatch developer, Boris Schaeling says:

[jBatch](http://iomash.com/) comes with a `wandbox` command to access the API more easily. To compile and run a Python program with Wandbox and append the program's output to a webpage, use:

```
<script type="text/jbatch">
  wandbox run --compiler python-2.7.3 --code 'print("Hello, world!")' | appendTo body
</script>
```

See [CodePen](http://codepen.io/iomash/pen/KwBEJG) for Wandbox with jBatch in action.


### Boost.SML


[Boost.SML document](https://boost-experimental.github.io/sml/examples.html) is using Wandbox on [background](https://github.com/boost-experimental/sml/blob/758ceb8646cb2eb56f2e121021c29fab55f24e92/js/cpp.js#L51)

### Boost.DI

[Boost.DI document](https://boost-experimental.github.io/di/examples.html) is also using Wandbox on background.

### Stensal

[Stensal](https://stensal.com/) is a C/C++ SDK tool to help developers find the cause of segmentation faults in a more intuitive way.
It is using Wandbox on background.
