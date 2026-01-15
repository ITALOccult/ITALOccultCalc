using System;
using System.Text;

namespace Occult
{
	internal class AsteroidPrepoint_Stars : IComparable
	{
		private double tOffset;

		private double mv;

		private string rA;

		private string dec;

		private string decDiff;

		private string sao = "";

		public double Mv
		{
			set
			{
				mv = value;
			}
		}

		public string RA
		{
			set
			{
				rA = value;
			}
		}

		public string Dec
		{
			set
			{
				dec = value;
			}
		}

		public string SAO
		{
			set
			{
				sao = value;
			}
		}

		public double TOffset
		{
			get
			{
				return tOffset;
			}
			set
			{
				tOffset = value;
			}
		}

		public string DecDiff
		{
			set
			{
				decDiff = value;
			}
		}

		public int CompareTo(object other)
		{
			return tOffset.CompareTo(((AsteroidPrepoint_Stars)other).tOffset);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.DEGtoDMS(tOffset, 2, 0, MinutesOnly: false));
			stringBuilder.AppendFormat("{0,7:F1}  ", mv);
			stringBuilder.Append(rA + "  ");
			stringBuilder.Append(dec);
			stringBuilder.Append(decDiff);
			if (sao.Trim() == "0")
			{
				stringBuilder.Append("".PadLeft(7));
			}
			else
			{
				stringBuilder.Append(sao.PadLeft(7));
			}
			return stringBuilder.ToString();
		}

		public string ToString(double baseTime)
		{
			StringBuilder stringBuilder = new StringBuilder();
			double num = baseTime - tOffset;
			if (num < 0.0)
			{
				num += 24.0;
			}
			if (num > 24.0)
			{
				num -= 24.0;
			}
			stringBuilder.Append(Utilities.DEGtoDMS(num, 2, 0, MinutesOnly: false));
			stringBuilder.AppendFormat("{0,7:F1}  ", mv);
			stringBuilder.Append(rA + "  ");
			stringBuilder.Append(dec);
			stringBuilder.Append(decDiff);
			stringBuilder.Append(sao.PadLeft(7));
			return stringBuilder.ToString();
		}
	}
}
