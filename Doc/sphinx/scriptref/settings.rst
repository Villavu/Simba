.. _scriptref-settings:

Settings
========

Setting Functions
-----------------

DeleteSetting
~~~~~~~~~~~~~

.. code-block:: pascal

    function DeleteSetting(const KeyName: string): Boolean

DeleteSubSettings
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function DeleteSubSettings(const KeyName: string): Boolean

GetSettingValueDef
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetSettingValueDef(const KeyName, defVal: string): string

GetSettingValue
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetSettingValue(const KeyName: string): string

KeyIsDirectory
~~~~~~~~~~~~~~

.. code-block:: pascal

    function KeyIsDirectory(const KeyName: string): Boolean

KeyIsSetting
~~~~~~~~~~~~

.. code-block:: pascal

    function KeyIsSetting(const KeyName: string): Boolean

ListSettings
~~~~~~~~~~~~

.. code-block:: pascal

    function ListSettings(const KeyName: string; var KeyReturn: TstringArray): Boolean

SetSettingValue
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function SetSettingValue(const KeyName, value: string): Boolean
