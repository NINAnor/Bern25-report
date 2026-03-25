// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: doc => article(
  title: [Assessing the conservation status of Norwegian species and habitats for the Bern Convention in 2025],
  subtitle: [version 20.03.2026 (Draft)],
  authors: (
    ( name: [Balint Czucz],
      affiliation: [],
      email: [] ),
    ( name: [Duncan Halley],
      affiliation: [],
      email: [] ),
    ( name: [Kwaku Peprah Adjei],
      affiliation: [],
      email: [] ),
    ( name: [Fia Bengtsson],
      affiliation: [],
      email: [] ),
    ( name: [Kim Magnus Bærum],
      affiliation: [],
      email: [] ),
    ( name: [Joseph Chipperfield],
      affiliation: [],
      email: [] ),
    ( name: [Jiska van Dijk],
      affiliation: [],
      email: [] ),
    ( name: [Børre Dervo],
      affiliation: [],
      email: [] ),
    ( name: [Nina E. Eide],
      affiliation: [],
      email: [] ),
    ( name: [Anders Endrestøl],
      affiliation: [],
      email: [] ),
    ( name: [Duncan Halley],
      affiliation: [],
      email: [] ),
    ( name: [Hanne Hegre],
      affiliation: [],
      email: [] ),
    ( name: [Anders Kolstad],
      affiliation: [],
      email: [] ),
    ( name: [Jon H. Magerøy],
      affiliation: [],
      email: [] ),
    ( name: [Jan Ketil Rød],
      affiliation: [],
      email: [] ),
    ( name: [Heidi Solstad],
      affiliation: [],
      email: [] ),
    ( name: [Ole Gunnar Støen],
      affiliation: [],
      email: [] ),
    ( name: [Joachim Paul Töpper],
      affiliation: [],
      email: [] ),
    ( name: [Signe Nybø],
      affiliation: [],
      email: [] ),
    ),
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Context and purpose
<context-and-purpose>
The Bern Convention, adopted in 1979, is an international treaty aimed at conserving wild plant and animal species and their natural habitats in several non-EU European and North-African countries. It legally binds its member countries to protect over 1,000 animal species and 500 plant species, focusing on species and habitats that require cross-border cooperation. A relatively new amendment (Resolution 8, 2012) extended the Bern Convention with a network of Emerald sites and the implementation of management, monitoring, and reporting measures for them. The Emerald Network is the Pan-European equivalent of the EU's Natura 2000 network, and this resolution provides the framework for how countries designate and manage these ecological sites. In Norway the Emerald sites largely (but not entirely) correspond to the other existing protected areas (see #ref(<fig-bgr-emerald>, supplement: [Figure])).

#figure([
#box(image("../img/bgr_emerald.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
The four EU biogeographical regions (BGR), and the Emerald Network in Norway
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-bgr-emerald>


According to #emph[Resolution 8] (2012), every country must perform regular reporting about a selection of #emph[species] and #emph[habitat types];. The structure and content of the reporting, as well as its timing follows very closely the so-called "Article 17 reporting" of the EU Habitats Directive. Furthermore, both reporting processes have a #emph[6-year cycle];, which means that for the Bern Convention countries the 2025 reporting is their second reporting altogether (the first, relatively limited reporting took place in 2019). The main purpose of the whole reporting is to assess the "#emph[conservation status];" of the selected species and habitats in a semi-standardised way, based on the best available data and evidence.

= The scope of the reporting
<the-scope-of-the-reporting>
The heart of the Resolution 8 reporting consists of a series of detailed assessments that address a number of pre-selected, species, habitat types and #emph[biogeographic regions] (BGR, see #ref(<fig-bgr-emerald>, supplement: [Figure])). For 2025 Norway received a "checklist" consisting of 74 #emph[species] and 6 #emph[habitat types];, which are often also called #emph[features] collectively. Each of these features was pre-selected for #emph[assessment] in one or more of the four EU biogeographic regions (BGR) that intersect the territory of the country.#footnote[In the context of the Bern Convention, assessing one feature (a species or a habitat type) in one region is counted as "one assessment".] Eventually, the presence of two species was not confirmed by the experts,#footnote[Stag beetle (#emph[Lucanus cervus];) was never observed in Norway, and the orchid #emph[Liparis loeselei] was extinct too long ago to be relevant for the Bern Convention.] and the list of biogeographic regions also had to be updated for several species. This resulted in a final set 157 #emph[full] and 9 #emph[partial] assessments covering 72 #emph[species] 6 #emph[habitat types] across the four biogeographic regions of Norway (#ref(<tbl-fgrp>, supplement: [Table])).#footnote[Partial assessments have a significantly limited data content without conclusions. Such assessments are allowed for species and habitats only #emph[marginally] present in a biogeographic region (MAR), as well as for #emph[occasionally] occurring (OCC) and #emph[newly arriving] species (ARR).] In addition to The full list of the species assessed and the main assessment conclusions will be presented in an Annex (TODO!!).

#figure([
#table(
  columns: 4,
  align: (auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Feature groups], [#set text(weight: "bold"); N#sub[features];], [#set text(weight: "bold"); N#sub[assmt.full];], [#set text(weight: "bold"); N#sub[assmt.part];],),
  table.hline(),
  table.cell(align: top)[Mammals (ma)], table.cell(align: top)[7], table.cell(align: top)[25], table.cell(align: top)[0],
  table.cell(align: top)[Fishes & amphibians (fa)], table.cell(align: top)[6], table.cell(align: top)[15], table.cell(align: top)[0],
  table.cell(align: top)[Insects and molluscs (im)], table.cell(align: top)[14], table.cell(align: top)[23], table.cell(align: top)[3],
  table.cell(align: top)[Vascular plants (vp)], table.cell(align: top)[33], table.cell(align: top)[47], table.cell(align: top)[5],
  table.cell(align: top)[Mosses (mo)], table.cell(align: top)[12], table.cell(align: top)[30], table.cell(align: top)[1],
  table.cell(align: top)[Habitat types (ht)], table.cell(align: top)[6], table.cell(align: top)[17], table.cell(align: top)[0],
  table.cell(align: top)[Total], table.cell(align: top)[78], table.cell(align: top)[157], table.cell(align: top)[9],
)
], caption: figure.caption(
position: top, 
[
Groups of features (species, habitats) used in this report (N#sub[features];: number of features, N#sub[assmt.full];: number of full assessments, N#sub[assmt.part];: number of partial assessments)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-fgrp>


= Conservation status of species and habitat types
<conservation-status-of-species-and-habitat-types>
We start with presenting an overview of the conclusions of the assessments. Each assessment has an overall conclusion, which is based on four "parameters", each of which has its own conclusions. The conclusions for the individual parameters rely on quantitative and qualitative data collected by taxonomic experts following detailed guidelines provided by the Bern Secretariat. The four assessment parameters for species include its #emph[distribution range] (Ran), #emph[population size] (Pop), the quality and quantity of the #emph[available habitat] for the species (H4s), and its #emph[future prospects] (Fpr) based on the balance of pressures and conservation measures. All conclusions are evaluated in a simple "traffic light system" using one of the following four #emph[conservation status] categories:

- #strong[FV];: Favourable (green),
- #strong[U1];: Unfavourable - inadequate (amber),
- #strong[U2];: Unfavourable - bad (red),
- #strong[XX];: Unknown (grey).

== Species assessments
<species-assessments>
#ref(<tbl-conclusions-sp-fgrp>, supplement: [Table]) provides an overview of the overall conclusions across the major species groups, whereas #ref(<tbl-conclusions-sp-para>, supplement: [Table]) shows the distribution of the partial conclusions for the individual parameters across all the species.

#figure([
#table(
  columns: 5,
  align: (auto,auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Species groups], [#set text(weight: "bold"); FV], [#set text(weight: "bold"); U1], [#set text(weight: "bold"); U2], [#set text(weight: "bold"); XX],),
  table.hline(),
  table.cell(align: top)[Mammals (ma)], table.cell(align: top)[3], table.cell(align: top)[20], table.cell(align: top)[2], table.cell(align: top)[---],
  table.cell(align: top)[Fishes & amphibians (fa)], table.cell(align: top)[---], table.cell(align: top)[3], table.cell(align: top)[3], table.cell(align: top)[9],
  table.cell(align: top)[Insects and molluscs (im)], table.cell(align: top)[3], table.cell(align: top)[6], table.cell(align: top)[8], table.cell(align: top)[6],
  table.cell(align: top)[Vascular plants (vp)], table.cell(align: top)[1], table.cell(align: top)[26], table.cell(align: top)[18], table.cell(align: top)[2],
  table.cell(align: top)[Mosses (mo)], table.cell(align: top)[5], table.cell(align: top)[15], table.cell(align: top)[10], table.cell(align: top)[---],
  table.cell(align: top)[All (all)], table.cell(align: top)[12], table.cell(align: top)[70], table.cell(align: top)[41], table.cell(align: top)[17],
)
], caption: figure.caption(
position: top, 
[
Distribution of overall conclusions across the different groups of features (FV: Favourable; U1: Unfavourable - inadequate; U2: Unfavourable - bad; XX: Unknown)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-conclusions-sp-fgrp>


#figure([
#table(
  columns: 5,
  align: (auto,auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Assessment parameters], [#set text(weight: "bold"); FV], [#set text(weight: "bold"); U1], [#set text(weight: "bold"); U2], [#set text(weight: "bold"); XX],),
  table.hline(),
  table.cell(align: top)[Range (Ran)], table.cell(align: top)[48], table.cell(align: top)[53], table.cell(align: top)[14], table.cell(align: top)[25],
  table.cell(align: top)[Population (Pop)], table.cell(align: top)[15], table.cell(align: top)[83], table.cell(align: top)[11], table.cell(align: top)[31],
  table.cell(align: top)[Habitat for species (H4s)], table.cell(align: top)[41], table.cell(align: top)[50], table.cell(align: top)[32], table.cell(align: top)[17],
  table.cell(align: top)[Future prospects (Fpr)], table.cell(align: top)[14], table.cell(align: top)[68], table.cell(align: top)[38], table.cell(align: top)[20],
)
], caption: figure.caption(
position: top, 
[
Distribution of partial conclusions over the different parameters (FV: Favourable; U1: Unfavourable - inadequate; U2: Unfavourable - bad; XX: Unknown)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-conclusions-sp-para>


Below we also visualise the distribution of the conclusions across the different species groups and assessment parameters (#ref(<fig-sp-conclusions>, supplement: [Figure])).

#figure([
#grid(columns: 1, gutter: 2em,
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_1a-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_1b-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_2a-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_2b-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_2c-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_2d-1.svg"))

],
)
], caption: figure.caption(
position: bottom, 
[
The conclusions of the species assessments in the 2025 reporting of Norway to the Bern Convention (Resultion 8). Top row: overall conclusions for all species (pie chart) and for major species groups (columns). Bottom row: partial conclusions for the four main assessment parameters.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-sp-conclusions>


== Habitat type assessments
<habitat-type-assessments>
#ref(<tbl-conclusions-ha-para>, supplement: [Table]) and #ref(<fig-ha-conclusions>, supplement: [Figure]) provide an overview of the different conclusions for the habitat types assessed in 2025 in Norway.

#figure([
#table(
  columns: 5,
  align: (auto,auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Assessment parameters], [#set text(weight: "bold"); FV], [#set text(weight: "bold"); U1], [#set text(weight: "bold"); U2], [#set text(weight: "bold"); XX],),
  table.hline(),
  table.cell(align: top)[Range (Ran)], table.cell(align: top)[15], table.cell(align: top)[---], table.cell(align: top)[2], table.cell(align: top)[---],
  table.cell(align: top)[Area of habitats (Are)], table.cell(align: top)[---], table.cell(align: top)[9], table.cell(align: top)[8], table.cell(align: top)[---],
  table.cell(align: top)[Structure and function (SnF)], table.cell(align: top)[---], table.cell(align: top)[5], table.cell(align: top)[12], table.cell(align: top)[---],
  table.cell(align: top)[Future prospects (Fpr)], table.cell(align: top)[---], table.cell(align: top)[5], table.cell(align: top)[12], table.cell(align: top)[---],
  table.cell(align: top)[Overall conclusion (OvC)], table.cell(align: top)[---], table.cell(align: top)[5], table.cell(align: top)[12], table.cell(align: top)[---],
)
], caption: figure.caption(
position: top, 
[
Distribution of partial conclusions over the different parameters (FV: Favourable; U1: Unfavourable - inadequate; U2: Unfavourable - bad; XX: Unknown)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-conclusions-ha-para>


#figure([
#grid(columns: 1, gutter: 2em,
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_ha1-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/conclusions_ha2-1.svg"))

],
)
], caption: figure.caption(
position: bottom, 
[
The conclusions of habitat type assessments in the 2025 reporting of Norway to the Bern Convention (Resultion 8). Left: overall conclusions for all habitat types (pie chart); right: partial conclusions for the four main assessment parameters (Ran: range, Are: area, SnF: structure and function, Fpr: future prospects)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-ha-conclusions>


= Changes and trends
<changes-and-trends>
The assessment protocol offers several ways to describe past, present and future trends in the conservation status of the studied species and habitats. This includes the assessment of the #emph[short-term] and #emph[long-term] trends by the experts participating in the 2025 assessment, as well as a direct comparisons of the consecutive assessments (e.g comparing the conservation status of the same species & habitats assessed in 2019 and 2025). In the context of the Bern Convention #emph[short-term trends] always refer to the past 12 years (i.e.~2013-2024 in this case), whereas #emph[long term trends] are referring to the 24 year period before the assessment date (i.e.~2001-2024). In addition, the parameter #emph[future prospects] discussed in the previous section also addresses trends, synthesizing expectations for changes in the other three parameters during the forthcoming 12 years (i.e.~2025-2036).

In the next few sections we provide an overview of the main current and past trends relying on the different elements of the Bern assessment protocol.

== Short term trends
<short-term-trends>
Short term trends are first assessed individually for the different state parameters (range, population size, and habitat availability for species, and range, area, and structure/function for habitat types), and then the directions of these elementary trends are aggregated into an #emph[overall trend qualifier];, which is the second flagship output of Bern Convention assessments, providing critical insights for the interpretation of the #emph[overall conservation status];. In #ref(<fig-trend-qualifiers>, supplement: [Figure]) we provide an overview of these trend qualifiers for the different groups of features.

#figure([
#grid(columns: 1, gutter: 2em,
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_1a-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_1b-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_2_ran_calc-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_2_pop-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_2_are-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_2_h4s-1.svg"))

],
  [
#box(image("bern25_report_main_files/figure-typst/qualifiers_2_snf-1.svg"))

],
)
], caption: figure.caption(
position: bottom, 
[
Overall short-term (2013-2024) trends identified during the 2025 assessments. Top row: the overall "#emph[trend qualifiers];" among all assessments (pie chart) and the distribution of trend types across the main groups of features (columns). Bottom row: short term trends for the main assessment parameters. (ma: mammals, fa= fishes & amphibians, im= insects and molluscs, vp: vascular plants, mo: mosses, ht: habitat types; Ran: range, Pop: population, Are: area, H4s: habitat for species, SnF: structure and function)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-trend-qualifiers>


== Long term trends
<long-term-trends>
In this reporting round, #emph[long-term trends] were only assessed for some of the parameters (range & population size for species, and range and area for habitat types), and only in cases when the expert were long-term evidence was available. These cases means that… … #strong[?\@tbl-xxxx] provides an overview of the … (TODO!)

== Changes since 2019
<changes-since-2019>
The 2019 reporting period was the first reporting period for Norway under Resolution 8 of the Bern Convention. This reporting period was considered to be a pilot exercise by the Bern Convention Secretariat, so only very few species and habitat types were selected for reporting. #ref(<tbl-changes-19>, supplement: [Table]) provides an overview of the changes in the overall assessment conclusions for the overlapping species.

#figure([
#table(
  columns: 3,
  align: (auto,auto,auto,),
  table.header([#set text(weight: "bold"); Change], [#set text(weight: "bold"); N], [#set text(weight: "bold"); Assessments],),
  table.hline(),
  table.cell(align: top)[U1 --\> U1], table.cell(align: top)[5], table.cell(align: top)[Gulflekktorvlibelle (ATL); marisko (ALP, ATL, BOR); rikmyr (ALP)],
  table.cell(align: top)[U2 --\> U2], table.cell(align: top)[5], table.cell(align: top)[Alvemose (ATL, BOR); eremitt (BOR); rikmyr (ATL, BOR)],
  table.cell(align: top)[FV --\> U1], table.cell(align: top)[4], table.cell(align: top)[Oter (ALP, ARC, ATL, BOR)],
  table.cell(align: top)[XX --\> U1], table.cell(align: top)[4], table.cell(align: top)[Brunbjørn (ALP, BOR); ulv (ALP, BOR)],
  table.cell(align: top)[U1 --\> U2], table.cell(align: top)[3], table.cell(align: top)[Alvemose (ALP); lågurtedellauvskog (ATL, BOR)],
  table.cell(align: top)[XX --\> XX], table.cell(align: top)[3], table.cell(align: top)[Bekkeniøye (ATL, BOR); smalknøttsnegl (BOR)],
  table.cell(align: top)[U2 --\> U1], table.cell(align: top)[2], table.cell(align: top)[Myrsildre (ALP, ATL)],
  table.cell(align: top)[FV --\> XX], table.cell(align: top)[1], table.cell(align: top)[Hvitfinnet steinulk (BOR)],
  table.cell(align: top)[U1 --\> FV], table.cell(align: top)[1], table.cell(align: top)[Gulflekktorvlibelle (BOR)],
)
], caption: figure.caption(
position: top, 
[
Changes in the overall conclusions of the repeated assessments between 2019 and 2025 (FV: Favourable; U1: Unfavourable - inadequate; U2: Unfavourable - bad; XX: Unknown)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-changes-19>


If the overall conservation status of a species/habitat changed since 2019 then the experts were were requested to assess the main reasons for the change. They could select more then one reason from a list that included #emph[genuine change];, changes in #emph[data] or #emph[methods];. The results are summarised in #ref(<tbl-change-reasons-19>, supplement: [Table]).

#figure([
#table(
  columns: 3,
  align: (auto,auto,auto,),
  table.header([#set text(weight: "bold"); Reasons for status change between 2019 & 2025], [#set text(weight: "bold"); N], [#set text(weight: "bold"); Assessments],),
  table.hline(),
  table.cell(align: top)[Genuine change], table.cell(align: top)[4], table.cell(align: top)[Gulflekktorvlibelle (ATL, BOR); lågurtedellauvskog (ATL, BOR)],
  table.cell(align: top)[Improved knowledge/more accurate data], table.cell(align: top)[4], table.cell(align: top)[Oter (ALP, ARC, ATL, BOR)],
  table.cell(align: top)[Other reasons], table.cell(align: top)[18], table.cell(align: top)[Brunbjørn (ALP, ARC, ATL, BOR); gaupe (ALP, ARC, ATL, BOR); gulflekktorvlibelle (ATL, BOR); jerv (ALP, ARC, ATL, BOR); ulv (ALP, ARC, ATL, BOR)],
)
], caption: figure.caption(
position: top, 
[
The main reasons for changes in the overall conclusions since 2019
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-change-reasons-19>


= Background factors
<background-factors>
A major element of the Bern Convention (Resolution 8) reporting protocol is the assessment of #emph[pressures] (i.e.~threats) and #emph[conservation measures] affecting the status and trends of the selected species and habitat types. These factors have a key role in the assessment of the #emph[future prospects] parameter, but and they can also provide relevant information by themselves, e.g.~for effective conservation planning. The assessment protocol offers a long list of predefined types of pressures and conservation measures, and the experts could choose from these lists, assigning an ordered list of #emph[pressures] and #emph[measures] to each species and habitat type.

== Key pressures
<key-pressures>
Beyond just listing the relevant pressures the experts also had to characterise them from three different perspectives: its timing (past, ongoing or expected future pressure), extent (the share of the population/area it affects), and influence (the degree of impact on the affected population/area). #ref(<tbl-press-list>, supplement: [Table]) presents an overview of the pressures that were most commonly listed as "ongoing" threats across all species and habitat types.

#figure([
#table(
  columns: (auto, 80%, auto, auto),
  align: (auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Code], [#set text(weight: "bold"); Pressure name], [#set text(weight: "bold"); N], [#set text(weight: "bold"); #block[
    #set text(size: 9pt); Feature groups
    ]
    #block[
    #set text(size: 6pt , weight: "regular"); #block[
    ma
    ]
    #block[
    fa
    ]
    #block[
    im
    ]
    #block[
    vp
    ]
    #block[
    mo
    ]
    #block[
    ht
    ]
    ]],),
  table.hline(),
  table.cell(align: top)[PE01], table.cell(align: top)[Roads, paths, railroads and related infrastructure], table.cell(align: top)[41], table.cell(align: horizon + top)[#block[
  19 0 19 1 1 5 4 11
  ]],
  table.cell(align: top)[PI03], table.cell(align: top)[Problematic native species], table.cell(align: top)[41], table.cell(align: horizon + top)[#block[
  33 0 2 2 0 33 0 4
  ]],
  table.cell(align: top)[PF01], table.cell(align: top)[Conversion from other land uses to built-up areas], table.cell(align: top)[37], table.cell(align: horizon + top)[#block[
  13 0 3 3 5 3 10 13
  ]],
  table.cell(align: top)[PJ03], table.cell(align: top)[Changes in precipitation regimes due to climate change], table.cell(align: top)[34], table.cell(align: horizon + top)[#block[
  15 0 2 2 4 15 6 5
  ]],
  table.cell(align: top)[PJ01], table.cell(align: top)[Temperature changes and extremes due to climate change], table.cell(align: top)[30], table.cell(align: horizon + top)[#block[
  16 0 0 4 3 16 3 4
  ]],
  table.cell(align: top)[PB09], table.cell(align: top)[Clear-cutting, removal of all trees], table.cell(align: top)[28], table.cell(align: horizon + top)[#block[
  13 0 0 0 7 6 13 2
  ]],
  table.cell(align: top)[PD02], table.cell(align: top)[Hydropower (dams, weirs, run-off-the-river and respective infrastructure)], table.cell(align: top)[28], table.cell(align: horizon + top)[#block[
  8 0 4 8 3 4 4 5
  ]],
  table.cell(align: top)[PJ10], table.cell(align: top)[Change of habitat location, size, and / or quality due to climate change], table.cell(align: top)[28], table.cell(align: horizon + top)[#block[
  22 0 0 0 2 22 4 0
  ]],
  table.cell(align: top)[PF03], table.cell(align: top)[Creation or development of sports, tourism and leisure infrastructure], table.cell(align: top)[27], table.cell(align: horizon + top)[#block[
  19 0 19 0 0 1 4 3
  ]],
  table.cell(align: top)[PA05], table.cell(align: top)[Abandonment of management/use of grasslands and other agricultural and agroforestry systems (e.g. cessation of grazing, mowing or traditional farming)], table.cell(align: top)[23], table.cell(align: horizon + top)[#block[
  13 0 0 0 3 13 3 4
  ]],
  table.cell(align: top)[PF13], table.cell(align: top)[Drainage, land reclamation and conversion of wetlands, marshes, bogs, etc. for built-up areas], table.cell(align: top)[22], table.cell(align: horizon + top)[#block[
  9 0 6 0 3 0 4 9
  ]],
  table.cell(align: top)[PG11], table.cell(align: top)[Illegal shooting/killing], table.cell(align: top)[20], table.cell(align: horizon + top)[#block[
  20 0 20 0 0 0 0 0
  ]],
  table.cell(align: top)[PB24], table.cell(align: top)[Drainage for forestry], table.cell(align: top)[19], table.cell(align: horizon + top)[#block[
  14 0 0 0 3 2 14 0
  ]],
  table.cell(align: top)[PB06], table.cell(align: top)[Logging or thinning (excluding clear cutting)], table.cell(align: top)[18], table.cell(align: horizon + top)[#block[
  13 0 0 0 4 1 13 0
  ]],
  table.cell(align: top)[PA13], table.cell(align: top)[Application of natural or synthetic fertilisers on agricultural land], table.cell(align: top)[16], table.cell(align: horizon + top)[#block[
  11 0 0 1 11 0 4 0
  ]],
  table.cell(align: top)[PG08], table.cell(align: top)[Hunting], table.cell(align: top)[16], table.cell(align: horizon + top)[#block[
  16 0 16 0 0 0 0 0
  ]],
  table.cell(align: top)[PA07], table.cell(align: top)[Intensive grazing or overgrazing by livestock], table.cell(align: top)[15], table.cell(align: horizon + top)[#block[
  8 0 0 0 7 8 0 0
  ]],
  table.cell(align: top)[PB02], table.cell(align: top)[Conversion from one type of forestry land use to another], table.cell(align: top)[13], table.cell(align: horizon + top)[#block[
  11 0 0 0 2 0 11 0
  ]],
  table.cell(align: top)[PB23], table.cell(align: top)[Physical alteration of water bodies for forestry (including dams)], table.cell(align: top)[13], table.cell(align: horizon + top)[#block[
  9 0 0 0 4 0 9 0
  ]],
  table.cell(align: top)[PI02], table.cell(align: top)[Other invasive alien species (other than species of Bern Convention concern)], table.cell(align: top)[12], table.cell(align: horizon + top)[#block[
  8 0 0 1 2 1 0 8
  ]],
)
], caption: figure.caption(
position: top, 
[
The ten most most commmon active (ongoing) pressures identified during the assessments (N: number of assessments where the given pressure was identified; ma: mammals, fa= fishes & amphibians, im= insects and molluscs, vp: vascular plants, mo: mosses, ht: habitat types)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-press-list>


== Main conservation measures
<main-conservation-measures>
Similarly to the pressures, the experts also identified the morst important conservation measures for each feature in each biogeographic region. This included all "ongoing" (actively implemented) conservation measures, as well as any clearly useful, but not yet implemented measures. In addition, for each already implemented measure the experts also documented if the measures were implemented only within the Emerald sites, or also beyond them. #ref(<tbl-meas-list>, supplement: [Table]) presents an overview of the most frequently applied conservation measures.

#figure([
#table(
  columns: (auto, 80%, auto, auto),
  align: (auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Code], [#set text(weight: "bold"); Conservation measure name], [#set text(weight: "bold"); N], [#set text(weight: "bold"); #block[
    #set text(size: 9pt); Feature groups
    ]
    #block[
    #set text(size: 6pt , weight: "regular"); #block[
    ma
    ]
    #block[
    fa
    ]
    #block[
    im
    ]
    #block[
    vp
    ]
    #block[
    mo
    ]
    #block[
    ht
    ]
    ]],),
  table.hline(),
  table.cell(align: top)[MG02], table.cell(align: top)[Management of hunting, recreational fishing, and the recreational or commercial harvesting or collection of plants and fungi (incl. restoration of habitats)], table.cell(align: top)[7], table.cell(align: horizon + top)[#block[
  4 0 0 3 4 0 0 0
  ]],
  table.cell(align: top)[MS04], table.cell(align: top)[Manage native species (incl. species which are not protected by the Bern Convention / Resolution No. 6 (1998))], table.cell(align: top)[5], table.cell(align: horizon + top)[#block[
  4 0 1 0 4 0 0 0
  ]],
  table.cell(align: top)[MA01], table.cell(align: top)[Prevent conversion of natural and semi-natural habitats, and habitats of species into agricultural land], table.cell(align: top)[4], table.cell(align: horizon + top)[#block[
  4 0 0 0 4 0 0 0
  ]],
  table.cell(align: top)[MA03], table.cell(align: top)[Maintain existing extensive agricultural practices and agricultural landscape features], table.cell(align: top)[4], table.cell(align: horizon + top)[#block[
  3 0 0 0 1 3 0 0
  ]],
  table.cell(align: top)[MG01], table.cell(align: top)[Management of professional/commercial fishing, shellfish and seaweed harvesting (incl. restoration of habitats)], table.cell(align: top)[4], table.cell(align: horizon + top)[#block[
  3 0 0 3 1 0 0 0
  ]],
  table.cell(align: top)[MI03], table.cell(align: top)[Management, control or eradication of other invasive alien species], table.cell(align: top)[4], table.cell(align: horizon + top)[#block[
  2 0 0 1 0 1 0 2
  ]],
  table.cell(align: top)[MS01], table.cell(align: top)[Reinforce populations of species from the Resolution No. 6 (1998)], table.cell(align: top)[4], table.cell(align: horizon + top)[#block[
  3 0 1 0 3 0 0 0
  ]],
  table.cell(align: top)[MB14], table.cell(align: top)[Manage drainage and water abstraction for forestry (inc. restoration of drained or hydrologically altered habitats)], table.cell(align: top)[3], table.cell(align: horizon + top)[#block[
  3 0 0 0 0 0 0 3
  ]],
  table.cell(align: top)[MC04], table.cell(align: top)[Reduce impact of hydropower operation and infrastructure (incl. the restoration of freshwater habitats)], table.cell(align: top)[3], table.cell(align: horizon + top)[#block[
  3 0 0 3 0 0 0 0
  ]],
  table.cell(align: top)[ME06], table.cell(align: top)[Habitat restoration of areas impacted by transport], table.cell(align: top)[3], table.cell(align: horizon + top)[#block[
  3 0 0 0 0 0 0 3
  ]],
)
], caption: figure.caption(
position: top, 
[
The ten most frequently implemented conservation measures, as identified during the assessments (N: number of assessments where the given measure was identified; ma: mammals, fa= fishes & amphibians, im= insects and molluscs, vp: vascular plants, mo: mosses, ht: habitat types)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-meas-list>


== Role of the Emerald Network
<role-of-the-emerald-network>
Protected areas, and in particular the Emerald Network is assumed to provide substantial support for endangered species and habitats. Several parts of the Bern assessment explore the influence of the Emerald Network, thus providing useful feedback for conservation management. In #ref(<tbl-eme-trends>, supplement: [Table]) we compare #emph[within Emerald] short-term trends of several parameters with the general trends of the same parameters within the entire biogeographic region. This comparison is only sensitive to the direction of the trend, so significant differences between slight and drastic declines (or improvements) can still be marked as "same within/beyond Emerald" in #ref(<tbl-eme-trends>, supplement: [Table]).

#figure([
#table(
  columns: 5,
  align: (auto,auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Trends within/beyond Emerald], [#set text(weight: "bold"); Population (Pop)], [#set text(weight: "bold"); Habitat for species (H4s)], [#set text(weight: "bold"); Area of habitats (Are)], [#set text(weight: "bold"); Structure and function (SnF)],),
  table.hline(),
  [Better #emph[within] Emerald], [1], [2], [15], [13],
  [Same trend within/beyond], [72], [83], [2], [4],
  [Better #emph[beyond] Emerald], [3], [1], [---], [---],
  [Unknown (at least partly)], [64], [54], [---], [---],
)
], caption: figure.caption(
position: top, 
[
Comparison of short term trends in several parameters within the Emerald network and outside of it.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-eme-trends>


#block[
```r
# A quick temporary overview of the most interesting cases of the table above:
tmp2 %>% 
  filter(para %in% c("Pop", "H4s"), cc2 %in% c("better", "worse")) %>%
  mutate(species=map_chr(spp, \(x) str_c(x, collapse=", "))) %>%
  select(para, cc2, species) %>%
  print
```

#block[
```
# A tibble: 4 × 3
  para  cc2    species                  
  <chr> <fct>  <chr>                    
1 H4s   better grønnsko, russeburkne    
2 Pop   better grønnsko                 
3 H4s   worse  oter                     
4 Pop   worse  gulflekktorvlibelle, oter
```

]
]
In addition we can also compare the implementation of the conservation measures within and beyond the Emerald Network based on the answers of the experts. #ref(<tbl-eme-meas>, supplement: [Table]) offers such a comparison.

#figure([
#table(
  columns: 5,
  align: (auto,auto,auto,auto,auto,),
  table.header([#set text(weight: "bold"); Code], [#set text(weight: "bold"); Conservation measure name], [#set text(weight: "bold"); N#sub[in];], [#set text(weight: "bold"); N#sub[i.o];], [#set text(weight: "bold"); N#sub[out];],),
  table.hline(),
  table.cell(align: top)[MG02], table.cell(align: top)[Management of hunting, recreational fishing, and the recreational or commercial harvesting or collection of plants and fungi (incl. restoration of habitats)], table.cell(align: top)[0], table.cell(align: top)[5], table.cell(align: top)[2],
  table.cell(align: top)[MS04], table.cell(align: top)[Manage native species (incl. species which are not protected by the Bern Convention / Resolution No. 6 (1998))], table.cell(align: top)[1], table.cell(align: top)[2], table.cell(align: top)[2],
  table.cell(align: top)[MA01], table.cell(align: top)[Prevent conversion of natural and semi-natural habitats, and habitats of species into agricultural land], table.cell(align: top)[0], table.cell(align: top)[0], table.cell(align: top)[4],
  table.cell(align: top)[MA03], table.cell(align: top)[Maintain existing extensive agricultural practices and agricultural landscape features], table.cell(align: top)[0], table.cell(align: top)[3], table.cell(align: top)[1],
  table.cell(align: top)[MG01], table.cell(align: top)[Management of professional/commercial fishing, shellfish and seaweed harvesting (incl. restoration of habitats)], table.cell(align: top)[0], table.cell(align: top)[2], table.cell(align: top)[2],
  table.cell(align: top)[MI03], table.cell(align: top)[Management, control or eradication of other invasive alien species], table.cell(align: top)[0], table.cell(align: top)[3], table.cell(align: top)[1],
  table.cell(align: top)[MS01], table.cell(align: top)[Reinforce populations of species from the Resolution No. 6 (1998)], table.cell(align: top)[1], table.cell(align: top)[0], table.cell(align: top)[3],
  table.cell(align: top)[MB14], table.cell(align: top)[Manage drainage and water abstraction for forestry (inc. restoration of drained or hydrologically altered habitats)], table.cell(align: top)[0], table.cell(align: top)[0], table.cell(align: top)[3],
  table.cell(align: top)[MC04], table.cell(align: top)[Reduce impact of hydropower operation and infrastructure (incl. the restoration of freshwater habitats)], table.cell(align: top)[0], table.cell(align: top)[2], table.cell(align: top)[1],
  table.cell(align: top)[ME06], table.cell(align: top)[Habitat restoration of areas impacted by transport], table.cell(align: top)[0], table.cell(align: top)[0], table.cell(align: top)[3],
)
], caption: figure.caption(
position: top, 
[
The location of the most frequently implemented conservation measures: predominantly within Emerald sites (N#sub[in];), mostly outside of the Emerald Network (N#sub[out];), or both within and beyond Emerald (N#sub[i.o];)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-eme-meas>


= Bevaring av fjellreven en suksess -- tidobling av bestanden
<bevaring-av-fjellreven-en-suksess-tidobling-av-bestanden>
Fjellreven har lenge vært kritisk truet i Norge. Arten ble fredet allerede i 1930, men fortsatte å gå tilbake. Bestanden var trolig allerede da for liten til å respondere positivt på vern, samtidig ble det observert store endring i smågnagerdynamikken og økt forekomst av rødrev på tundraen, som kan ha påvirket fjellreven negativt. Rundt 2000 antok man at bestanden bare talte 40-60 voksne individer i Norge, Sverige og Finland til sammen. Arten var da svært nær å bli utryddet. Med bakgrunn i den første handlingsplanen for arten, kom tiltakene i Norge i gang fra 2005. Avlsprogrammet for fjellrev ble da etablert på fjellet og har per nå satt ut over 500 fjellrev. I tillegg til å reetablere fem utdødde norske delbestander har utsatte fjellrever bidratt til å styrke mange av de gjenværende delbestandene i Fennoskandia og også medført reetablering i Finland. Det ble da også satt i gang støttefôring, og det er per nå mer enn 150 fôrautomater i drift gjennom hele året i så godt som hele utbredelsen. Det tredje tiltaket, systematisk uttak av rødrev, gjøres bare i den nordligste delen av utbredelsen, og ellers bare der rødreven er i direkte konkurranse med fjellreven. Tiltakene har resultert i en tidobling av bestanden, nå beregnet til å være over 300 voksne fjellrev i Norge. Som følge av det ble fjellreven listet til sterkt truet i 2021 (#ref(<fig-fjellrev-bestand>, supplement: [Figure])).

I Norge finnes fjellreven bare i det alpine og arktiske biomet, hvorav 85-90% i det alpine tundrabiomet. Bare 10-15% av bestanden finnes utenfor Emerald nettverket. Beskyttelsen verneområdene er gitt, blant annet gjennom strenge restriksjoner på utbygging av infrastruktur, anlegg og bygninger har trolig bidratt sterkt til at det fortsatt er bæreevne for fjellrev i disse områdene. Restriksjoner på utbygging i store deler av tundrabiomet gjør at områdene beholder sin naturlige marginalitet som høyfjellsartene er særdeles godt tilpasset. Økt menneskelig aktivitet fører alltid med seg høyere tetthet av generalistarter, blant annet rødrev, som kan fortrenge fjellreven om den opptrer i høye tettheter.

#figure([
#box(image("../img/fjellrev_bestand.png", width: 90.0%))
], caption: figure.caption(
position: bottom, 
[
Estimert bestandsstørrelse (med 95 % konfidensintervaller) for fjellrev i Norge i perioden 2008--2025, basert på en lukket fangst-gjenfangst modell bygget på innsamlet DNA-materiale i den samme perioden. Rød linje viser glidende gjennomsnitt over 3-årsperioder. Merk at gjennomsnittses-timatene er sentrert, dvs. estimatet for 2023--2025 vises som estimat på 2024 i figuren. (Fra Jackson mfl. 2025)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-fjellrev-bestand>


Vern av leveområdene, omfang og ikke minst varigheten av tiltakene har vært avgjørende for suksessen av bevaringsarbeidet. Tross suksessen og tidoblingen, er fjellrevbestanden fortsatt langt fra å være levedyktig, blant annet på grunn av lav konnektivitet og utfordringer med innavl. Forskerne anslår at tiltakene må vedvare med samme intensitet 10-15 år til, før man kan begynne å trappe ned arbeidet. Bevaringsarbeidet er finansiert av Miljødirektoratet, tidvis med bidrag fra InterReg. Det praktiske arbeidet gjøres i hovedsak av Statens naturoppsyn, NINA har ansvar for den faglige koordineringen av tiltakene og overvåking av bestanden, samt drift av avlsprogrammet (#ref(<fig-fjellrev-photo>, supplement: [Figure])).

#figure([
#box(image("../img/fjellrev_photo.jpeg", width: 90.0%))
], caption: figure.caption(
position: bottom, 
[
Utsetting av fjellrev i Hardangervidda nasjonalpark, en del av Emerald Nettverket (Foto: Toralf Mjøen, Avlsprogrammet, NINA)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-fjellrev-photo>


= Data availability
<data-availability>
(?) TODO/TBC…

= References
<references>
- Jackson, C., Ulvund, K., Rød-Eriksen L., Furnes, M.W., Arntsen, L., Mjøen, T., Spets, M., Jensen, M.Ø., Kleven, O., Flagstad, Ø. & Eide, N.E. 2025. Fjellrev i Norge 2025 - Tiltak, status og anbe-falinger. NINA Rapport 2698. Norsk institutt for naturforskning.
- TODO…

= Annexes
<annexes>
- A table with all species & habitats (incl latin names) and their main conclusions
