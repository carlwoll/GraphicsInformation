(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Nov 22, 2017 *)

BeginPackage["GraphicsInformation`"]
(* Exported symbols added here with SymbolName::usage *) 

GraphicsInformation::usage = "GraphicsInformation[graphics] returns sizing information about the graphics object"

Begin["`Private`"]
(* Implementation of the package *)

GraphicsInformation[gr_Graphics] := Replace[
    GraphicsInformation[{gr}],
    Rule[a_, {b_}] :> a -> b,
    {1}
]

GraphicsInformation[gr:{__Graphics}] := Module[{info, res},
    info = Flatten @ Reap[
        Rule @@@ ReplaceAll[
            "Regions",
            FrontEndExecute @ ExportPacket[
                toNotebook[gr],
                "BoundingBox",
                Verbose->True
            ]
        ],
        _,
        #1->#2[[1]]&
    ];
    res = extract[info] /@ Range @ Length @ gr;
    Thread @ Rule[
        {"ImagePadding", "ImageSize", "PlotRangeSize", "ImagePaddingSize", "PlotRange", "BasePlotRange"},
        Thread @ ReplaceAll[
            {"ImagePadding", "ImageSize", "PlotRangeSize", "ImagePaddingSize", "PlotRange", "BasePlotRange"},
            res
        ]
    ]
]

toNotebook[gr_] := Notebook[
    Cell[BoxData @ ToBoxes @ #, "Output"]& /@ instrumentGraphics[gr],
    WindowSize -> CurrentValue[EvaluationNotebook[], WindowSize],
    Evaluator -> CurrentValue[EvaluationNotebook[], Evaluator]
]

instrumentGraphics[gr:{__Graphics}] := Join[
	MapThread[
	    Show[#1,
	        GridLines -> {sowRange["X" -> #2], sowRange["Y" -> #2]},
	        Epilog -> {
	            Annotation[
	                Rectangle[Scaled[{0,0}], Scaled[{1,1}]],
	                "PlotRange", #2
	            ],
	            Annotation[
	                Rectangle[ImageScaled[{0,0}], ImageScaled[{1,1}]],
	                "ImageSize", #2
	            ]
	        }
	    ]&,
	    {gr, Range@Length@gr}
	],
	MapThread[
		Show[#,
			PlotRangePadding->0,
	        GridLines -> {sowRange["BX" -> #2], sowRange["BY" -> #2]}
	    ]&,
	    {gr, Range@Length@gr}
	]
]

instrumentGraphics[gr_Graphics] := instrumentGraphics[{gr}]

sowRange[label_] := Function[Sow[{##}, label]; None]

extract[rules_][k_] := Module[{pr, is, xr, yr, uxr, uyr},
    {pr, is, xr, yr, uxr, uyr} = {{"PlotRange",k}, {"ImageSize",k}, "X"->k, "Y"->k, "BX"->k, "BY"->k} /. rules;
    {
    "ImagePadding"->Abs[is-pr],
    "ImageSize"->Abs[Subtract@@@is],
    "PlotRangeSize"->Abs[Subtract@@@pr],
    "ImagePaddingSize"->Total[Abs[is-pr],{2}],
    "PlotRange"->{xr,yr},
    "BasePlotRange"->{uxr, uyr}
    }
]

End[]

EndPackage[]

