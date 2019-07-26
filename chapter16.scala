// chapter 16

// 1
val a = <fred/>   // scala.xml.Elem = <fred/>
val b = a(0)      // scala.xml.Node = <fred/>
val c = b(0)      // scala.xml.Node = <fred/>
a == b
b == c            // true
// <fred/> is an Elem, which is a Node, which is a NodeSeq with one element: <fred/>

// 2 - that causes a parse error which can be resolved by doubling the braces:
<ul>
  <li>Opening bracket: [</li>
  <li>Closing bracket: ]</li>
  <li>Opening brace: {{</li>
  <li>Closing brace: }}</li>
</ul>

// 3
import scala.xml._
import scala.xml.dtd.DocType
<li>Fred</li> match { case <li>{Text(t)}</li> => t }
<li>{"Fred"}</li> match { case <li>{e}</li> => e.getClass } // e's type is scala.xml.Atom
<li>{Text("Fred")}</li> match { case <li>{Text(t)}</li> => t } // to resolve

// 4
import scala.xml.parsing.XhtmlParser
val source = scala.io.Source.fromFile("~/scala/index.html", "UTF-8")
val parser = new XhtmlParser(source)
val doc = parser.initialize.document
for (img <- doc \\ "img" if img.attribute("alt").isEmpty) {
  println(img)
}

// 5
for (img <- doc \\ "img"; src <- img.attribute("src")) {
  println(src)
}

// 6
for (a <- doc \\ "a"; href <- a.attribute("href")) {
  println(s"${a.text}:\t$href")
}

// 7
def mapToDl(defs: Map[String,String]): Elem =
  <dl>{ defs.map { case (k,v) => <dt>{k}</dt><dd>{v}</dd> } }</dl>

val dl = mapToDl(Map("A" -> "1", "B" -> "2"))

// 8
def dlToMap(dl: Elem): Map[String,String] =
  ((dl \\ "dt").map(_.text) zip (dl \\ "dd").map(_.text)).toMap

dlToMap(dl)

// 9
import scala.xml.transform.{RewriteRule, RuleTransformer}
val addAlt = new RewriteRule {
  override def transform(n: Node) = n match {
    case img @ <img/> if img.attribute("alt").isEmpty =>
      img.asInstanceOf[Elem] % Attribute(null, "alt", "TODO", Null)
    case _ => n
  }
}
val doc2 = new RuleTransformer(addAlt).transform(doc)

// 10
println("dtd.externalID = " + doc.dtd.externalID)
println("dtd.attr = " + doc.dtd.attr)
XML.save("~/scala/index2.html", doc2(0), enc = "UTF-8",
  doctype = DocType("html", doc.dtd.externalID, Nil))
