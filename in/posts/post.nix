\{ post, html, map, string } ->

let
  inherit (html) meta title link head body header div main h1;

  google-fonts = [
    "Inconsolata"
    "Merriweather"
  ];

  google-font-url = "https://fonts.googleapis.com/css?family=${string.intercalate "|" google-fonts}";

in
  html.document [
    (head [
      (meta { charset = "utf-8"; })
      (meta {
        name = "viewport";
        content = "width=device-width,initial-scale=1";
      })
      (link { rel = "stylesheet"; href = google-font-url; })
      (title post.title)
      (link { rel = "icon"; href = ""; })
      (link { rel = "stylesheet"; href = post.css; })
      (map meta post.meta)
    ])
    (body {} [
      (header { class = "global-page-header"; }
        (H.div { class = "container"; }
          (a { href = ".."; } "Chris Martin" )
        )
      )
      (main (div [
        (div { class = "post-head container"; } [
          (h1 { class = "post-title"; } post.title)
          (div { class = "post-date"; } post.date)
        ])
        (div { class = "post-body"; } post.body)
      ]))
    ])
  ]
