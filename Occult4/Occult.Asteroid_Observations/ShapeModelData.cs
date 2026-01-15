using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class ShapeModelData : IComparable
	{
		private string[] FitOptions = new string[8] { "Not fitted", "Bad occn data", "Model wrong", "Minimum dia", "Dia but no fit", "Poor fit", "Good fit", "Not constrained" };

		private string source = "";

		private string iD = "";

		private string version = "";

		private double diaMin;

		private double diaMax;

		private double surfaceVolumeRatio = 1.0;

		private double phaseCorrn;

		private int fitQuality;

		public string Source
		{
			get
			{
				return source;
			}
			set
			{
				source = value;
			}
		}

		public string ID
		{
			get
			{
				return iD;
			}
			set
			{
				iD = value;
			}
		}

		public double SurfaceVolumeRatio
		{
			get
			{
				return surfaceVolumeRatio;
			}
			set
			{
				surfaceVolumeRatio = value;
			}
		}

		public int FitQuality
		{
			get
			{
				return fitQuality;
			}
			set
			{
				fitQuality = value;
			}
		}

		public double PhaseCorrn
		{
			get
			{
				return phaseCorrn;
			}
			set
			{
				phaseCorrn = value;
			}
		}

		public double DiaMin
		{
			get
			{
				return diaMin;
			}
			set
			{
				diaMin = value;
			}
		}

		public double DiaMax
		{
			get
			{
				return diaMax;
			}
			set
			{
				diaMax = value;
			}
		}

		public string Version
		{
			get
			{
				return version;
			}
			set
			{
				version = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Source.CompareTo(((ShapeModelData)other).Source) == 0)
			{
				return ID.CompareTo(((ShapeModelData)other).ID);
			}
			return Source.CompareTo(((ShapeModelData)other).Source);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Source.PadRight(7));
			stringBuilder.Append("#" + ID.PadRight(6));
			stringBuilder.AppendFormat("{0,7:f3}    ", SurfaceVolumeRatio);
			stringBuilder.Append(FitOptions[FitQuality].PadRight(15));
			stringBuilder.Append(PhaseCorrn.ToString().PadLeft(4) + "° ");
			if ((DiaMin == 0.0) & (DiaMax == 0.0))
			{
				stringBuilder.Append(" ...  ...  ");
			}
			else if (DiaMin < 30.0)
			{
				stringBuilder.AppendFormat("{0,4:f1} {1,4:f1}  ", DiaMin, DiaMax);
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:f0} {1,4:f0}  ", DiaMin, DiaMax);
			}
			stringBuilder.Append(Version);
			return stringBuilder.ToString();
		}

		public string Summary()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Source.PadRight(5) + "#" + ID.PadRight(6));
			stringBuilder.AppendFormat(" {0,4:F0}km <> {1,4:f0}km,  Phs{2,4:f0}°, Q = {3,1:f0}", DiaMin, DiaMax, PhaseCorrn, FitQuality);
			return stringBuilder.ToString();
		}
	}
}
