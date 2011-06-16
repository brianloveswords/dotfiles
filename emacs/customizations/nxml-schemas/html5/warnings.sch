<?xml version="1.0"?>
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<!-- RELAX NG Schema for HTML 5: Warnings                          -->
<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
<schema xmlns='http://www.ascc.net/xml/schematron'>
  <ns prefix='h' uri='http://www.w3.org/1999/xhtml'/>
  <pattern name="Warnings for HTML5 attributes that are obsolete but conforming">
    <rule context='h:img'>
      <report test='@border' role='warning'>
        The &#x201C;border&#x201D; attribute on the &#x201C;img&#x201D; element is obsolete.
        Consider specifying &#x201C;img { border: 0; }&#x201C; in CSS instead.
      </report>
    </rule>
    <rule context='h:script[translate(@language, "JAVSCRIPT", "javscript")="javascript"]'>
      <report test='not(@type) or translate(@type, "EXJAVSCRIPT", "exjavscript")="text/javascript"' role='warning'>
        The &#x201C;language&#x201D; attribute on the &#x201C;script&#x201D; element is obsolete. You can safely omit it.
      </report>
    </rule>
    <rule context='h:a'>
      <report test='@name' role='warning'>
        The &#x201C;name&#x201D; attribute on the &#x201C;a&#x201D; element is obsolete. Consider putting an
        &#x201C;id&#x201D; attribute on the nearest container instead.
      </report>
    </rule>
  </pattern>
  <pattern name="Other warnings">
    <rule context='h:video|h:audio'>
      <report test='count(h:track[@default]) > 1' role='warning'>
        &#x201C;<name/>&#x201D; element has more than one &#x201C;track&#x201D;
        child element with a &#x201C;default&#x201D; attribute.
      </report>
    </rule>
  </pattern>
</schema>
