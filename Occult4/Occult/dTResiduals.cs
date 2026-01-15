using System;

namespace Occult
{
	internal class dTResiduals : IComparable
	{
		private double year;

		private double residualInTime;

		private double residualArcSec;

		private double deltaT_Initial;

		private double sDev;

		private int numObs;

		internal double Year
		{
			get
			{
				return year;
			}
			set
			{
				year = value;
			}
		}

		internal double TimeCorrection
		{
			get
			{
				return residualInTime;
			}
			set
			{
				residualInTime = value;
			}
		}

		internal double ResidualInTimeAbs => Math.Abs(residualInTime);

		internal double ResidualArcSec
		{
			get
			{
				return residualArcSec;
			}
			set
			{
				residualArcSec = value;
			}
		}

		internal double DeltaT_Initial
		{
			get
			{
				return deltaT_Initial;
			}
			set
			{
				deltaT_Initial = value;
			}
		}

		internal double DeltaT_Corrected => deltaT_Initial + residualInTime;

		internal double SDev
		{
			get
			{
				return sDev;
			}
			set
			{
				sDev = value;
			}
		}

		internal int NumObs
		{
			get
			{
				return numObs;
			}
			set
			{
				numObs = value;
			}
		}

		public int CompareTo(object other)
		{
			return ResidualInTimeAbs.CompareTo(((dTResiduals)other).ResidualInTimeAbs);
		}

		internal string CorrectedValues()
		{
			return string.Format("{0,6:f1}: {1,6:f1}: {2,4:f1}: {3,4:f1}: {4,3:f0}", Year, DeltaT_Corrected, TimeCorrection, SDev, NumObs);
		}
	}
}
