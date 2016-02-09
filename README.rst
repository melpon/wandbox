Wandbox
=======

Wandbox is a social compilation service.

URL to this service is http://melpon.org/wandbox

You can also access via API: https://github.com/melpon/wandbox/blob/master/kennel2/API.rst

These programs are licensed by Boost Software License 1.0.

Wandbox on Your Editor
----------------------

You can use Wandbox on your editor.

Vim
  https://github.com/rhysd/wandbox-vim

Emacs
  https://github.com/kosh04/emacs-wandbox

xyzzy
  https://gist.github.com/kikairoya/7544234

Wandbox as a Background Infrastructure
--------------------------------------

You can use Wandbox API from another services (that is, Wandbox API's HTTP response header has ``Access-Control-Allow-Origin: *``).

Following services are using Wandbox on background.

jBatch
~~~~~~

jBatch developer, Boris Schaeling says

  jBatch_ comes with a ``wandbox`` command to access the API more easily. To compile and run a Python program with Wandbox and append the program's output to a webpage, use::

    <script type="text/jbatch">
      wandbox run --compiler python-2.7.3 --code 'print("Hello, world!")' | appendTo body
    </script>
  
  See CodePen_ for Wandbox with jBatch in action.

.. _jBatch: http://iomash.com/
.. _CodePen: http://codepen.io/iomash/pen/KwBEJG
