using System;
using System.Text;

namespace Occult
{
	public class DoubleStarSepPA : IComparable
	{
		private double magA;

		private double magB;

		private double sep;

		private double pA;

		private double meanRatio;

		private string wds = "";

		private string wDS_Name = "";

		public string WDS_Name
		{
			get
			{
				return wDS_Name;
			}
			set
			{
				wDS_Name = value;
			}
		}

		public string WDS_ID
		{
			get
			{
				return wds;
			}
			set
			{
				wds = value;
			}
		}

		public double Sep
		{
			get
			{
				return sep;
			}
			set
			{
				sep = value;
			}
		}

		public double PA
		{
			get
			{
				return pA;
			}
			set
			{
				pA = value;
			}
		}

		public double MagA
		{
			get
			{
				return magA;
			}
			set
			{
				magA = value;
			}
		}

		public double MagB
		{
			get
			{
				return magB;
			}
			set
			{
				magB = value;
			}
		}

		public double MeanRatio
		{
			get
			{
				return meanRatio;
			}
			set
			{
				meanRatio = value;
			}
		}

		public int CompareTo(object other)
		{
			return sep.CompareTo(((DoubleStarSepPA)other).Sep);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (wDS_Name.Contains("OCc"))
			{
				stringBuilder.Append("**");
			}
			stringBuilder.Append(WDS_ID);
			if (MagA > -5.0)
			{
				if (meanRatio != 1.0)
				{
					stringBuilder.AppendFormat(" {0,4:F2}", MagA);
				}
				else
				{
					stringBuilder.AppendFormat(" {0,3:F1}", MagA);
				}
			}
			else
			{
				stringBuilder.Append("    ");
			}
			if (MagB > -5.0)
			{
				if (meanRatio != 1.0)
				{
					stringBuilder.AppendFormat(" {0,4:F2}", MagB);
				}
				else
				{
					stringBuilder.AppendFormat(" {0,3:F1}", MagB);
				}
			}
			else
			{
				stringBuilder.Append("     ");
			}
			if (sep > 30.0)
			{
				stringBuilder.AppendFormat(" {0:F0}\"", sep);
			}
			else if ((meanRatio != 1.0) & (sep > 0.07))
			{
				stringBuilder.AppendFormat(" {0:F2}\"", sep);
			}
			else if (sep > 0.7)
			{
				stringBuilder.AppendFormat(" {0:F1}\"", sep);
			}
			else if (sep > 0.07)
			{
				stringBuilder.AppendFormat(" {0:F2}\"", sep);
			}
			else if (sep > 0.0)
			{
				stringBuilder.AppendFormat(" {0:F3}\"", sep);
			}
			else
			{
				stringBuilder.Append("     ");
			}
			if (PA > -1.0)
			{
				stringBuilder.AppendFormat("{0,6:F1}", pA);
			}
			else
			{
				stringBuilder.Append("      ");
			}
			return stringBuilder.ToString();
		}

		public string ReductionString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(WDS_ID.PadRight(6));
			stringBuilder.AppendFormat("{0,8:F3}", sep);
			stringBuilder.AppendFormat("{0,7:F2}", pA);
			stringBuilder.AppendFormat("{0,7:F4}", meanRatio);
			return stringBuilder.ToString();
		}
	}
}
