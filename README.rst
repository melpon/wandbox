Wandbox
=======

Wandbox is a social compilation service.

URL to this service is http://melpon.org/wandbox

You can also access via API: https://github.com/melpon/wandbox/blob/master/kennel2/API.rst

These programs licensed by Boost Software License 1.0.

jBatch_ comes with a ``wandbox`` command to access the API more easily. To compile and run a Python program with Wandbox and append the program's output to a webpage, use::

  <script type="text/jbatch">
    wandbox run --compiler python-2.7.3 --code 'print("Hello, world!")' | appendTo body
  </script>

See CodePen_ for Wandbox with jBatch in action.

.. _jBatch: http://iomash.com/
.. _CodePen: http://codepen.io/iomash/pen/KwBEJG
