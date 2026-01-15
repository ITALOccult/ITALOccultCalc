namespace Occult.Asteroid_Observations
{
	internal class StarDiameterData
	{
		private float[] observedD = new float[600];

		private float[] observedR = new float[600];

		private string[] observedDTime = new string[600];

		private string[] observedRTime = new string[600];

		private int dcount;

		private int rcount;

		private string observer = "";

		private float dAdjust;

		private float rAdjust;

		private float frameRate = 25f;

		private float secondsToDisplay = 1f;

		private int isPartial;

		private int limbDarken;

		private string dMotion = "1";

		private string rMotion = "1";

		private int dRateUncertainty = 1;

		private int rRateUncertainty = 1;

		private int top1D;

		private int top2D;

		private int bottom1D;

		private int bottom2D;

		private int meas1D;

		private int meas2D;

		private int top1R;

		private int top2R;

		private int bottom2R;

		private int bottom1R;

		private int meas1R;

		private int meas2R;

		private double topDSD;

		private double bottomDSD;

		private double topRSD;

		private double bottomRSD;

		private int saturationLevelIndex;

		private double diaD;

		private double diaR;

		private int weightD;

		private int weightR;

		private string commentD = "";

		private string commentR = "";

		internal string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}

		internal float FrameRate
		{
			get
			{
				return frameRate;
			}
			set
			{
				frameRate = value;
			}
		}

		internal float SecondsToDisplay
		{
			get
			{
				return secondsToDisplay;
			}
			set
			{
				secondsToDisplay = value;
			}
		}

		internal int LimbDarken
		{
			get
			{
				return limbDarken;
			}
			set
			{
				limbDarken = value;
			}
		}

		internal int IsPartial
		{
			get
			{
				return isPartial;
			}
			set
			{
				isPartial = value;
			}
		}

		internal string DMotion
		{
			get
			{
				return dMotion;
			}
			set
			{
				dMotion = value;
			}
		}

		internal string RMotion
		{
			get
			{
				return rMotion;
			}
			set
			{
				rMotion = value;
			}
		}

		internal int DRateUncertainty
		{
			get
			{
				return dRateUncertainty;
			}
			set
			{
				dRateUncertainty = value;
			}
		}

		internal int RRateUncertainty
		{
			get
			{
				return rRateUncertainty;
			}
			set
			{
				rRateUncertainty = value;
			}
		}

		internal float DAdjust
		{
			get
			{
				return dAdjust;
			}
			set
			{
				dAdjust = value;
			}
		}

		internal float RAdjust
		{
			get
			{
				return rAdjust;
			}
			set
			{
				rAdjust = value;
			}
		}

		internal int DCount
		{
			get
			{
				return dcount;
			}
			set
			{
				dcount = value;
			}
		}

		internal int RCount
		{
			get
			{
				return rcount;
			}
			set
			{
				rcount = value;
			}
		}

		internal float[] ObservedD
		{
			get
			{
				return observedD;
			}
			set
			{
				observedD = value;
			}
		}

		internal float[] ObservedR
		{
			get
			{
				return observedR;
			}
			set
			{
				observedR = value;
			}
		}

		internal string[] ObservedDTime
		{
			get
			{
				return observedDTime;
			}
			set
			{
				observedDTime = value;
			}
		}

		internal string[] ObservedRTime
		{
			get
			{
				return observedRTime;
			}
			set
			{
				observedRTime = value;
			}
		}

		internal int Top1D
		{
			get
			{
				return top1D;
			}
			set
			{
				top1D = value;
			}
		}

		internal int Top2D
		{
			get
			{
				return top2D;
			}
			set
			{
				top2D = value;
			}
		}

		internal int Bottom1D
		{
			get
			{
				return bottom1D;
			}
			set
			{
				bottom1D = value;
			}
		}

		internal int Bottom2D
		{
			get
			{
				return bottom2D;
			}
			set
			{
				bottom2D = value;
			}
		}

		internal int Meas1D
		{
			get
			{
				return meas1D;
			}
			set
			{
				meas1D = value;
			}
		}

		internal int Meas2D
		{
			get
			{
				return meas2D;
			}
			set
			{
				meas2D = value;
			}
		}

		internal int Top1R
		{
			get
			{
				return top1R;
			}
			set
			{
				top1R = value;
			}
		}

		internal int Top2R
		{
			get
			{
				return top2R;
			}
			set
			{
				top2R = value;
			}
		}

		internal int Bottom1R
		{
			get
			{
				return bottom1R;
			}
			set
			{
				bottom1R = value;
			}
		}

		internal int Bottom2R
		{
			get
			{
				return bottom2R;
			}
			set
			{
				bottom2R = value;
			}
		}

		internal int Meas1R
		{
			get
			{
				return meas1R;
			}
			set
			{
				meas1R = value;
			}
		}

		internal int Meas2R
		{
			get
			{
				return meas2R;
			}
			set
			{
				meas2R = value;
			}
		}

		internal double TopDSD
		{
			get
			{
				return topDSD;
			}
			set
			{
				topDSD = value;
			}
		}

		internal double BottomDSD
		{
			get
			{
				return bottomDSD;
			}
			set
			{
				bottomDSD = value;
			}
		}

		internal double TopRSD
		{
			get
			{
				return topRSD;
			}
			set
			{
				topRSD = value;
			}
		}

		internal double BottomRSD
		{
			get
			{
				return bottomRSD;
			}
			set
			{
				bottomRSD = value;
			}
		}

		internal int SaturationLevelIndex
		{
			get
			{
				return saturationLevelIndex;
			}
			set
			{
				saturationLevelIndex = value;
			}
		}

		internal double DiaD
		{
			get
			{
				return diaD;
			}
			set
			{
				diaD = value;
			}
		}

		internal double DiaR
		{
			get
			{
				return diaR;
			}
			set
			{
				diaR = value;
			}
		}

		internal int WeightD
		{
			get
			{
				return weightD;
			}
			set
			{
				weightD = value;
			}
		}

		internal int WeightR
		{
			get
			{
				return weightR;
			}
			set
			{
				weightR = value;
			}
		}

		internal string CommentD
		{
			get
			{
				return commentD;
			}
			set
			{
				commentD = value;
			}
		}

		internal string CommentR
		{
			get
			{
				return commentR;
			}
			set
			{
				commentR = value;
			}
		}
	}
}
