namespace Occult.Asteroid_Observations
{
	internal class Tags
	{
		internal static readonly string ObservationsFileVersion = "2.15";

		internal static readonly string TagFileStart = "<AsteroidOccultations>";

		internal static readonly string TagFileEnd = "</AsteroidOccultations>";

		internal static readonly string[] TagStart = new string[36]
		{
			"<Event>", "<Date>", "<Details>", "<Star>", "<Asteroid>", "<SolveFlags>", "<EllipticFit>", "<EllipseUncertainty>", "<ShapeModelFit>", "<Fit>",
			"<Satellite>", "<Astrometry>", "<ReferenceTime>", "<MainBody>", "<Secondary>", "<MPC>", "<DoubleStar>", "<Solution>", "<Observations>", "<Prediction>",
			"<Observer>", "<ID>", "<Conditions>", "<D>", "<R>", "<Added>", "<LastEdited>", "<Editor>", "<Comments>", "<MainAtConjunction>",
			"<SatelliteFit>", "<SecondaryAtConjunction>", "<EventFits>", "<SatelliteBodies>", "<StarIssues>", "<JDSO>"
		};

		internal static readonly string[] TagEnd = new string[36]
		{
			"</Event>", "</Date>", "</Details>", "</Star>", "</Asteroid>", "</SolveFlags>", "</EllipticFit>", "</EllipseUncertainty>", "</ShapeModelFit>", "</Fit>",
			"</Satellite>", "</Astrometry>", "</ReferenceTime>", "</MainBody>", "</Secondary>", "</MPC>", "</DoubleStar>", "</Solution>", "</Observations>", "</Prediction>",
			"</Observer>", "</ID>", "</Conditions>", "</D>", "</R>", "</Added>", "</LastEdited>", "</Editor>", "</Comments>", "</MainAtConjunction>",
			"</SatelliteFit>", "</SecondaryAtConjunction>", "</EventFits>", "</SatelliteBodies>", "</StarIssues>", "</JDSO>"
		};

		internal static readonly string[] TagStart_Old = new string[2] { "<Primary>", "<AtConjunction>" };

		internal static readonly string[] TagEnd_Old = new string[2] { "</Primary>", "</AtConjunction>" };

		internal static int[] TagIndent = new int[36]
		{
			2, 4, 4, 6, 6, 8, 8, 8, 8, 10,
			10, 6, 8, 8, 10, 8, 8, 10, 4, 6,
			6, 8, 8, 8, 8, 4, 4, 4, 4, 8,
			8, 10, 6, 8, 8, 10
		};

		internal static int[] TagEditIndent = new int[36]
		{
			2, 4, 0, 0, 4, 4, 4, 4, 4, 6,
			4, 0, 0, 0, 0, 0, 4, 6, 0, 0,
			0, 0, 0, 0, 0, 4, 4, 2, 4, 0,
			4, 6, 4, 0, 0, 6
		};

		internal static readonly string[] chkReview_Labels = new string[4] { "set for review", "Unspecified reasons", "Satellite or double star ?", "Satellite or graze ?" };
	}
}
