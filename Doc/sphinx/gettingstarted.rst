Getting started with Simba
==========================

This page will help you install Simba and highlight some basic Simba
features.

Installing Simba
----------------

Installing Simba is pretty straighforward and will only take a couple of
minutes.


.. _simba-installer:

Simba installer
~~~~~~~~~~~~~~~

Everyone can download Simba at http://wizzup.org/simba/
Go to download, and download the Simba installer.

Once it is done downloading, simply run the installer and follow the steps.

.. note::
    Simba will install to C:/ by default; if you want to install it
    somewhere else, make sure you select a different location!

Simba will probably tell you there is an update available. In this case, an
update button will appear. Alternatively you can update Simba using
Tools -> Update.

.. note::
    Updating Simba regularly is recommended.

Simba Binary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is not the recommended method due to some computers having problems
with FreePascal and Lazarus. However, all you must do is install FreePascal
and Lazarus (from repos or non) and open the Simba project file located
in the repository (retrieve the project with git).
You should then see a prominent "compile" button. Yay!

Setting up SRL 4 with Simba
---------------------------

If you were using the :ref:`simba-installer` then you can simply enable the
``SRL Updater`` extension. (Go to View -> Extensions and enable srl.sex)

This is the only supported way. You can simply do a svn checkout on the srl
repository, but if you can do that, then you should be able to set up SRL
yourself as well.

Installing scripts
------------------

Scripts can be downloaded with the Script Manager, in Tools -> Script Manager.

.. note::
    The Script Manager is not finished yet.

Troubleshooting
---------------

If you run into problems that are not mentioned here, make sure you look at
the :ref:`Troubleshooting` page.
