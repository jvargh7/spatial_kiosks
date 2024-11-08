library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

diag <- grViz("
digraph PRISMA {
  
  graph [layout = dot, rankdir = TB]
  
  # Define nodes
  node [shape = rectangle, style = filled, color = lightgrey]
  A [label = 'Sessions from Jan. 2017 to Sept. 2024 \n (n = 91,896,297)']
  C [label = 'Sessions after initial screening (n = 86,774,937)']
  B [label = 'Sessions excluded: missing demographic \n or blood pressure information (n = 5,121,360)']
  E [label = 'Sessions with hypertension diagnosis (n = 1,356,654)']
  D [label = 'Sessions excluded: no hypertension diagnosis (n = 85,418,283)']
  G [label = 'Analytic dataset (n = 1,270,485)']
  F [label = 'Sessions excluded: duplicates (n = 86,169)']
  
  # Define edges
  A -> B
  A -> C
  C -> D
  C -> E
  E -> F
  E -> G
}
")

