using System.Text;

namespace Occult
{
	internal class LunarOccultationElements
	{
		private string starID;

		private string planetId;

		private string doubleCode;

		private string starVar;

		private string starSpectrum;

		private string starCert;

		private string zCName = "";

		private string variableDetails = "";

		private string doub1;

		private string doub2;

		private string doub3;

		private string doub4;

		private string doubID1;

		private string doubID2;

		private string doubID3;

		private string doubID4;

		private int xZNum;

		private int t;

		private int doubleCount;

		private int kepler2Cadence;

		private short planetNum;

		private long kepler2ID;

		private double mv;

		private double mp;

		private double mr;

		private double x;

		private double dx;

		private double d2x;

		private double d3x;

		private double d4x;

		private double y;

		private double dy;

		private double d2y;

		private double d3y;

		private double d4y;

		private double dMoon;

		private double d_dMoon;

		private double d2_dMoon;

		private double h;

		private double deltaH;

		private double deltaDec;

		private double jD;

		private double starRA;

		private double starDec;

		private double starRA2000;

		private double starDec2000;

		private double dia;

		private double pA1;

		private double sep1;

		private double pA2;

		private double sep2;

		private double pA3;

		private double sep3;

		private double pA4;

		private double sep4;

		private double pa_BrightLimb;

		private double illumination;

		private double phaseAngle;

		private double paPole;

		private double earthDec;

		private double shadowIncreaseOnFundamentalPlane;

		private double sinF2;

		private bool isMean1;

		private bool isMean2;

		private bool isMean3;

		private bool isMean4;

		private bool zCNameExists;

		private bool doubleDetailsExist;

		private bool variableDetailsExist;

		private bool observationsWanted;

		private bool nonInstantaneous;

		private bool lightCurveAvailable;

		internal string StarId
		{
			get
			{
				return starID;
			}
			set
			{
				starID = value;
			}
		}

		internal string PlanetId
		{
			get
			{
				return planetId;
			}
			set
			{
				planetId = value;
			}
		}

		internal string DoubleCode
		{
			get
			{
				return doubleCode;
			}
			set
			{
				doubleCode = value;
			}
		}

		internal string StarVar
		{
			get
			{
				return starVar;
			}
			set
			{
				starVar = value;
			}
		}

		internal string StarSpectrum
		{
			get
			{
				return starSpectrum;
			}
			set
			{
				starSpectrum = value;
			}
		}

		internal string StarCert
		{
			get
			{
				return starCert;
			}
			set
			{
				starCert = value;
			}
		}

		internal string ZCName
		{
			get
			{
				return zCName;
			}
			set
			{
				zCName = value;
				zCNameExists = true;
			}
		}

		internal string VariableDetails
		{
			get
			{
				return variableDetails;
			}
			set
			{
				variableDetails = value;
				variableDetailsExist = true;
			}
		}

		internal string Doub1
		{
			get
			{
				return doub1;
			}
			set
			{
				doub1 = value;
				doubleDetailsExist = true;
			}
		}

		internal string Doub2
		{
			get
			{
				return doub2;
			}
			set
			{
				doub2 = value;
			}
		}

		internal string Doub3
		{
			get
			{
				return doub3;
			}
			set
			{
				doub3 = value;
			}
		}

		internal string Doub4
		{
			get
			{
				return doub4;
			}
			set
			{
				doub4 = value;
			}
		}

		internal double PA1
		{
			get
			{
				return pA1;
			}
			set
			{
				pA1 = value;
			}
		}

		internal double PA2
		{
			get
			{
				return pA2;
			}
			set
			{
				pA2 = value;
			}
		}

		internal double PA3
		{
			get
			{
				return pA3;
			}
			set
			{
				pA3 = value;
			}
		}

		internal double PA4
		{
			get
			{
				return pA4;
			}
			set
			{
				pA4 = value;
			}
		}

		internal double Sep1
		{
			get
			{
				return sep1;
			}
			set
			{
				sep1 = value;
			}
		}

		internal double Sep2
		{
			get
			{
				return sep2;
			}
			set
			{
				sep2 = value;
			}
		}

		internal double Sep3
		{
			get
			{
				return sep3;
			}
			set
			{
				sep3 = value;
			}
		}

		internal double Sep4
		{
			get
			{
				return sep4;
			}
			set
			{
				sep4 = value;
			}
		}

		internal bool IsMean1
		{
			get
			{
				return isMean1;
			}
			set
			{
				isMean1 = value;
			}
		}

		internal bool IsMean2
		{
			get
			{
				return isMean2;
			}
			set
			{
				isMean2 = value;
			}
		}

		internal bool IsMean3
		{
			get
			{
				return isMean3;
			}
			set
			{
				isMean3 = value;
			}
		}

		internal bool IsMean4
		{
			get
			{
				return isMean4;
			}
			set
			{
				isMean4 = value;
			}
		}

		internal string DoubID1
		{
			get
			{
				return doubID1;
			}
			set
			{
				doubID1 = value;
			}
		}

		internal string DoubID2
		{
			get
			{
				return doubID2;
			}
			set
			{
				doubID2 = value;
			}
		}

		internal string DoubID3
		{
			get
			{
				return doubID3;
			}
			set
			{
				doubID3 = value;
			}
		}

		internal string DoubID4
		{
			get
			{
				return doubID4;
			}
			set
			{
				doubID4 = value;
			}
		}

		internal long Kepler2ID
		{
			get
			{
				return kepler2ID;
			}
			set
			{
				kepler2ID = value;
			}
		}

		internal int Kepler2Cadence
		{
			get
			{
				return kepler2Cadence;
			}
			set
			{
				kepler2Cadence = value;
			}
		}

		internal int XZNum
		{
			get
			{
				return xZNum;
			}
			set
			{
				xZNum = value;
			}
		}

		internal int T
		{
			get
			{
				return t;
			}
			set
			{
				t = value;
			}
		}

		internal int DoubleCount
		{
			get
			{
				return doubleCount;
			}
			set
			{
				doubleCount = value;
			}
		}

		internal bool ObservationsWanted
		{
			get
			{
				return observationsWanted;
			}
			set
			{
				observationsWanted = value;
			}
		}

		internal bool NonInstantaneous
		{
			get
			{
				return nonInstantaneous;
			}
			set
			{
				nonInstantaneous = value;
			}
		}

		internal double JDzero
		{
			get
			{
				return jD;
			}
			set
			{
				jD = value;
			}
		}

		internal double Mv
		{
			get
			{
				return mv;
			}
			set
			{
				mv = value;
			}
		}

		internal double Mp
		{
			get
			{
				return mp;
			}
			set
			{
				mp = value;
			}
		}

		internal double Mr
		{
			get
			{
				return mr;
			}
			set
			{
				mr = value;
			}
		}

		internal double X
		{
			get
			{
				return x;
			}
			set
			{
				x = value;
			}
		}

		internal double dX
		{
			get
			{
				return dx;
			}
			set
			{
				dx = value;
			}
		}

		internal double d2X
		{
			get
			{
				return d2x;
			}
			set
			{
				d2x = value;
			}
		}

		internal double d3X
		{
			get
			{
				return d3x;
			}
			set
			{
				d3x = value;
			}
		}

		internal double d4X
		{
			get
			{
				return d4x;
			}
			set
			{
				d4x = value;
			}
		}

		internal double Y
		{
			get
			{
				return y;
			}
			set
			{
				y = value;
			}
		}

		internal double dY
		{
			get
			{
				return dy;
			}
			set
			{
				dy = value;
			}
		}

		internal double d2Y
		{
			get
			{
				return d2y;
			}
			set
			{
				d2y = value;
			}
		}

		internal double d3Y
		{
			get
			{
				return d3y;
			}
			set
			{
				d3y = value;
			}
		}

		internal double d4Y
		{
			get
			{
				return d4y;
			}
			set
			{
				d4y = value;
			}
		}

		internal double DMoon
		{
			get
			{
				return dMoon;
			}
			set
			{
				dMoon = value;
			}
		}

		internal double dDMoon
		{
			get
			{
				return d_dMoon;
			}
			set
			{
				d_dMoon = value;
			}
		}

		internal double d2DMoon
		{
			get
			{
				return d2_dMoon;
			}
			set
			{
				d2_dMoon = value;
			}
		}

		internal double H
		{
			get
			{
				return h;
			}
			set
			{
				h = value;
			}
		}

		internal double DeltaH
		{
			get
			{
				return deltaH;
			}
			set
			{
				deltaH = value;
			}
		}

		internal double DeltaDec
		{
			get
			{
				return deltaDec;
			}
			set
			{
				deltaDec = value;
			}
		}

		internal double StarRA
		{
			get
			{
				return starRA;
			}
			set
			{
				starRA = value;
			}
		}

		internal double StarDec
		{
			get
			{
				return starDec;
			}
			set
			{
				starDec = value;
			}
		}

		internal double StarRA2000
		{
			get
			{
				return starRA2000;
			}
			set
			{
				starRA2000 = value;
			}
		}

		internal double StarDec2000
		{
			get
			{
				return starDec2000;
			}
			set
			{
				starDec2000 = value;
			}
		}

		internal double ShadowIncreaseOnFundamentalPlane
		{
			get
			{
				return shadowIncreaseOnFundamentalPlane;
			}
			set
			{
				shadowIncreaseOnFundamentalPlane = value;
			}
		}

		internal double SinF2
		{
			get
			{
				return sinF2;
			}
			set
			{
				sinF2 = value;
			}
		}

		internal double PlanetDiaArcSec
		{
			get
			{
				return dia;
			}
			set
			{
				dia = value;
			}
		}

		internal short PlanetNum
		{
			get
			{
				return planetNum;
			}
			set
			{
				planetNum = value;
			}
		}

		internal double PA_BrightLimb_deg
		{
			get
			{
				return pa_BrightLimb;
			}
			set
			{
				pa_BrightLimb = value;
			}
		}

		internal double Illumination
		{
			get
			{
				return illumination;
			}
			set
			{
				illumination = value;
			}
		}

		internal double PhaseAngle_deg
		{
			get
			{
				return phaseAngle;
			}
			set
			{
				phaseAngle = value;
			}
		}

		internal double PAPole_deg
		{
			get
			{
				return paPole;
			}
			set
			{
				paPole = value;
			}
		}

		internal double EarthDec_deg
		{
			get
			{
				return earthDec;
			}
			set
			{
				earthDec = value;
			}
		}

		internal bool ZCNameExists => zCNameExists;

		internal bool DoubleDetailsExist => doubleDetailsExist;

		internal bool VariableDetailsExist => variableDetailsExist;

		internal bool LightCurveAvailable
		{
			get
			{
				return lightCurveAvailable;
			}
			set
			{
				lightCurveAvailable = value;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(StarId);
			stringBuilder.AppendFormat("{0,6:F1}", mv);
			stringBuilder.AppendFormat("{0,9:F6}", x, dx, d2x, y, dy, d2y);
			return stringBuilder.ToString();
		}
	}
}
