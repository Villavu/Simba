
.. _scriptref_ocr:

OCR 
==============================
Simba has OCR functions (Optical Text Recognition); these are used to *read* text from an image.

It also has a wide variation of text *finding* functions. Both will be covered in
this article.


Fonts
-----

Fonts are an essential part of the text finding and identifying.
Now follows a brief explanation of the Font related functions in Simba.

LoadSystemFont
~~~~~~~~~~~~~~

.. code-block:: pascal

    function LoadSystemFont(const SysFont: TFont; const FontName: string):
    boolean;

This font loads a previously created Font with the name specified by
*FontName*.

Example:

.. code-block:: pascal

    program new;
    var
      Font : TFont;
    begin
      Font := TFont.Create;
      Font.Name := 'Courier New';
      Font.Size := 10;
      Font.Style := [];
      LoadSystemFont(Font,'test');
      DisplayDebugImgWindow(0,0);
      DisplayDebugImgWindow(150,50);
      DrawBitmapDebugImg(BitmapFromText('BMP[0] has not been freed','test'));
      Font.free;
    end.

