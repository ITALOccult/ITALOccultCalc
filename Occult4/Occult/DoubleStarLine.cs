using System;
using System.Text;

namespace Occult
{
	public class DoubleStarLine
	{
		private int xZa;

		private int xZb;

		private int xZmain;

		private int year1;

		private int year2;

		private int nObs;

		private int rAasNumber;

		private float mA = -10f;

		private float mB = -10f;

		private float pA1 = -10f;

		private float pA2 = -10f;

		private float d1 = -10f;

		private float d2 = -10f;

		private double rA;

		private double dec;

		private double period = -1.0;

		private double semiMajor = -1.0;

		private double inclination = -1.0;

		private double node = -1.0;

		private double tPeriastron = -1.0;

		private double periastron = -1.0;

		private double eccentricity = -1.0;

		private string iD;

		private string component;

		private string misc;

		private string wdsID = "".PadRight(10);

		private bool isMean;

		private const double Radian = 180.0 / Math.PI;

		private int arg1;

		private int arg2;

		private string arg3;

		public int XZa
		{
			get
			{
				return xZa;
			}
			set
			{
				xZa = value;
			}
		}

		public int XZb
		{
			get
			{
				return xZb;
			}
			set
			{
				xZb = value;
			}
		}

		public int XZmain
		{
			get
			{
				return xZmain;
			}
			set
			{
				xZmain = value;
			}
		}

		public bool IsMean
		{
			get
			{
				return isMean;
			}
			set
			{
				isMean = value;
			}
		}

		public string Component
		{
			get
			{
				return component;
			}
			set
			{
				component = value;
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

		public int RAasNumber => rAasNumber;

		public float MA => mA;

		public float MB => mB;

		public double SemiMajor => semiMajor;

		public double Eccentricity => eccentricity;

		public double Period => period;

		public string WDS_ID => wdsID;

		internal void ReadLine(string D)
		{
			if (!int.TryParse(D.Substring(0, 6), out xZa))
			{
				xZa = 0;
			}
			if (!int.TryParse(D.Substring(7, 6), out xZb))
			{
				xZb = 0;
			}
			if (!int.TryParse(D.Substring(14, 6), out xZmain))
			{
				xZmain = 0;
			}
			isMean = D.Substring(20, 1) == "M";
			if (!int.TryParse(D.Substring(21, 2), out arg1))
			{
				arg1 = 0;
			}
			if (!int.TryParse(D.Substring(23, 3), out arg2))
			{
				arg2 = 0;
			}
			rA = (double)arg1 + (double)arg2 / 600.0;
			rAasNumber = int.Parse(D.Substring(21, 5).Replace(' ', '0'));
			if (!int.TryParse(D.Substring(27, 2), out arg1))
			{
				arg1 = 0;
			}
			if (!int.TryParse(D.Substring(29, 2), out arg2))
			{
				arg2 = 0;
			}
			dec = (double)arg1 + (double)arg2 / 60.0;
			if (D.Substring(26, 1) == "-")
			{
				dec = 0.0 - dec;
			}
			wdsID = D.Substring(21, 10);
			iD = D.Substring(32, 7);
			component = D.Substring(40, 5);
			if (!int.TryParse(D.Substring(46, 4), out year1))
			{
				year1 = 0;
			}
			if (!int.TryParse(D.Substring(51, 4), out year2))
			{
				year2 = 0;
			}
			if (!int.TryParse(D.Substring(56, 2), out nObs))
			{
				nObs = 0;
			}
			if (!float.TryParse(D.Substring(59, 5), out pA1))
			{
				pA1 = -10f;
			}
			if (!float.TryParse(D.Substring(65, 5), out pA2))
			{
				pA2 = -10f;
			}
			if (!float.TryParse(D.Substring(71, 7), out d1))
			{
				d1 = -10f;
			}
			if (!float.TryParse(D.Substring(79, 7), out d2))
			{
				d2 = -10f;
			}
			if (!float.TryParse(D.Substring(87, 5), out mA))
			{
				mA = -10f;
			}
			if (!float.TryParse(D.Substring(93, 5), out mB))
			{
				mB = -10f;
			}
			misc = D.Substring(99, 32);
			if (!double.TryParse(D.Substring(132, 10), out period))
			{
				period = -1.0;
			}
			if (!double.TryParse(D.Substring(143, 9), out semiMajor))
			{
				semiMajor = -1.0;
			}
			if (!double.TryParse(D.Substring(153, 8), out inclination))
			{
				inclination = -1.0;
			}
			if (!double.TryParse(D.Substring(162, 8), out node))
			{
				node = -1.0;
			}
			if (!double.TryParse(D.Substring(171, 10), out tPeriastron))
			{
				tPeriastron = -1.0;
			}
			if (!double.TryParse(D.Substring(182, 7), out eccentricity))
			{
				eccentricity = -1.0;
			}
			if (!double.TryParse(D.Substring(190, 8), out periastron))
			{
				periastron = -1.0;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (xZa > 0)
			{
				stringBuilder.AppendFormat("{0,6:F0} ", xZa);
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			if (xZb > 0)
			{
				stringBuilder.AppendFormat("{0,6:F0} ", xZb);
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			if (xZmain > 0)
			{
				stringBuilder.AppendFormat("{0,6:F0}", xZmain);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (isMean)
			{
				stringBuilder.Append("M");
			}
			else
			{
				stringBuilder.Append(" ");
			}
			arg3 = Utilities.DEGtoDMS(rA, 2, 1, MinutesOnly: true);
			stringBuilder.Append((arg3.Substring(0, 2) + arg3.Substring(3, 2) + arg3.Substring(6, 1)).Replace(' ', '0'));
			if (dec < 0.0)
			{
				stringBuilder.Append("-");
			}
			else
			{
				stringBuilder.Append("+");
			}
			arg3 = Utilities.DEGtoDMS(Math.Abs(dec), 3, 0, MinutesOnly: true);
			stringBuilder.Append((arg3.Substring(1, 2) + arg3.Substring(4, 2)).Replace(' ', '0'));
			stringBuilder.Append(" " + iD + " " + component);
			if (year1 > 0)
			{
				stringBuilder.AppendFormat(" {0,4:F0}", year1);
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			if (year2 > 0)
			{
				stringBuilder.AppendFormat(" {0,4:F0}", year2);
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			if (nObs > 0)
			{
				stringBuilder.AppendFormat(" {0,2:F0}", nObs);
			}
			else
			{
				stringBuilder.Append("".PadRight(3));
			}
			if (pA1 > -1f)
			{
				stringBuilder.AppendFormat(" {0,5:F1}", pA1);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (pA2 > -1f)
			{
				stringBuilder.AppendFormat(" {0,5:F1}", pA2);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (d1 > -1f)
			{
				stringBuilder.AppendFormat(" {0,7:F3}", d1);
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (d2 > -1f)
			{
				stringBuilder.AppendFormat(" {0,7:F3}", d2);
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (mA > -10f)
			{
				stringBuilder.AppendFormat(" {0,5:F2}", mA);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (mB > -10f)
			{
				stringBuilder.AppendFormat(" {0,5:F2}", mB);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			stringBuilder.Append(" " + misc);
			if (period > -1.0)
			{
				stringBuilder.AppendFormat(" {0,10:F5}", period);
			}
			else
			{
				stringBuilder.Append("".PadRight(11));
			}
			if (semiMajor > -1.0)
			{
				stringBuilder.AppendFormat(" {0,9:F6}", semiMajor);
			}
			else
			{
				stringBuilder.Append("".PadRight(10));
			}
			if (inclination > -1.0)
			{
				stringBuilder.AppendFormat(" {0,8:F4}", inclination);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (node > -1.0)
			{
				stringBuilder.AppendFormat(" {0,8:F4}", node);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (tPeriastron > -1.0)
			{
				stringBuilder.AppendFormat(" {0,10:F5}", tPeriastron);
			}
			else
			{
				stringBuilder.Append("".PadRight(11));
			}
			if (eccentricity > -1.0)
			{
				stringBuilder.AppendFormat(" {0,7:F5}", eccentricity);
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (periastron > -1.0)
			{
				stringBuilder.AppendFormat(" {0,8:F4}", periastron);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			return stringBuilder.ToString();
		}

		internal string OrbitElements()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(iD + " " + component);
			if (period > -1.0)
			{
				stringBuilder.AppendFormat(" {0,10:F5}", period);
			}
			else
			{
				stringBuilder.Append("".PadRight(11));
			}
			if (semiMajor > -1.0)
			{
				stringBuilder.AppendFormat(" {0,9:F6}", semiMajor);
			}
			else
			{
				stringBuilder.Append("".PadRight(10));
			}
			if (inclination > -1.0)
			{
				stringBuilder.AppendFormat(" {0,8:F4}", inclination);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (node > -1.0)
			{
				stringBuilder.AppendFormat(" {0,8:F4}", node);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (tPeriastron > -1.0)
			{
				stringBuilder.AppendFormat(" {0,10:F5}", tPeriastron);
			}
			else
			{
				stringBuilder.Append("".PadRight(11));
			}
			if (eccentricity > -1.0)
			{
				stringBuilder.AppendFormat(" {0,7:F5}", eccentricity);
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (periastron > -1.0)
			{
				stringBuilder.AppendFormat(" {0,8:F4}", periastron);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			return stringBuilder.ToString();
		}

		internal string DoubleMainText()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(iD + " " + component);
			if (year1 > 0)
			{
				stringBuilder.AppendFormat(" {0,4:F0}", year1);
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			if (year2 > 0)
			{
				stringBuilder.AppendFormat(" {0,4:F0}", year2);
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			if (pA1 > -1f)
			{
				stringBuilder.AppendFormat(": {0,5:F1}", pA1);
			}
			else
			{
				stringBuilder.Append(":".PadRight(7));
			}
			if (pA2 > -1f)
			{
				stringBuilder.AppendFormat(" {0,5:F1}", pA2);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (d1 > -1f)
			{
				stringBuilder.AppendFormat(": {0,7:F3}", d1);
			}
			else
			{
				stringBuilder.Append(":".PadRight(9));
			}
			if (d2 > -1f)
			{
				stringBuilder.AppendFormat(" {0,7:F3}", d2);
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (mA > -10f)
			{
				stringBuilder.AppendFormat(": {0,5:F2}", mA);
			}
			else
			{
				stringBuilder.Append(":".PadRight(7));
			}
			if (mB > -10f)
			{
				stringBuilder.AppendFormat(" {0,5:F2}", mB);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (xZa > 0)
			{
				stringBuilder.Append(": " + XZ80Q.GetStarIDfromXZ(xZa));
			}
			else
			{
				stringBuilder.Append(":  ------");
			}
			if (xZb > 0)
			{
				stringBuilder.Append(" " + XZ80Q.GetStarIDfromXZ(xZb));
			}
			else
			{
				stringBuilder.Append("  ------");
			}
			if (isMean)
			{
				stringBuilder.Append(" mean");
			}
			return stringBuilder.ToString();
		}

		internal void PAandSep(double Year, out double PA, out double Sep)
		{
			double num = 0.0;
			PA = (Sep = -10.0);
			if (tPeriastron < 0.0)
			{
				double num2 = year2 - year1;
				if ((num2 <= 10.0) | (year1 < 1600))
				{
					PA = pA1;
					Sep = d1;
					return;
				}
				num = (Year - (double)year2) / num2;
				double num3 = ((!(pA2 < 0f)) ? ((double)(pA2 - pA1)) : 0.0);
				if (num3 < -180.0)
				{
					num3 += 360.0;
				}
				if (num3 > 180.0)
				{
					num3 -= 360.0;
				}
				if (Math.Abs(num * num3) > 180.0)
				{
					num = 0.0;
				}
				if (pA2 < 0f)
				{
					PA = pA1;
				}
				else
				{
					PA = (double)pA2 + num * num3;
					if (PA > 360.0)
					{
						PA -= 360.0;
					}
					if (PA < 0.0)
					{
						PA += 360.0;
					}
				}
				if (d2 < 0f)
				{
					Sep = d1;
				}
				else
				{
					Sep = (double)d2 + num * (double)(d2 - d1);
				}
			}
			else
			{
				double num4 = (Year - tPeriastron) / period % 1.0;
				if (num4 > 0.5)
				{
					num4 -= 1.0;
				}
				num4 = num4 * 2.0 * Math.PI;
				double num5 = num4;
				double num6;
				do
				{
					num6 = (num4 - num5 + eccentricity * Math.Sin(num5)) / (1.0 - eccentricity * Math.Cos(num5));
					num5 += num6;
				}
				while (Math.Abs(num6) > 0.0001);
				double num7 = 2.0 * Math.Atan(Math.Tan(num5 / 2.0) * Math.Sqrt((1.0 + eccentricity) / (1.0 - eccentricity)));
				double num8 = semiMajor * (1.0 - eccentricity * Math.Cos(num5));
				double num9 = num8 * Math.Sin(num7 + periastron / (180.0 / Math.PI)) * Math.Cos(inclination / (180.0 / Math.PI));
				double num10 = num8 * Math.Cos(num7 + periastron / (180.0 / Math.PI));
				for (PA = Math.Atan2(num9, num10) * (180.0 / Math.PI) + node; PA < 0.0; PA += 360.0)
				{
				}
				while (PA > 360.0)
				{
					PA -= 360.0;
				}
				Sep = Math.Sqrt(num9 * num9 + num10 * num10);
			}
		}

		internal void PAandSep_byOrbitFraction(double PeriodFraction, out double PA, out double Sep)
		{
			PA = (Sep = -10.0);
			if (!(tPeriastron < 0.0))
			{
				double num = PeriodFraction;
				if (num > 0.5)
				{
					num -= 1.0;
				}
				num = num * 2.0 * Math.PI;
				double num2 = num;
				double num3;
				do
				{
					num3 = (num - num2 + eccentricity * Math.Sin(num2)) / (1.0 - eccentricity * Math.Cos(num2));
					num2 += num3;
				}
				while (Math.Abs(num3) > 0.0001);
				double num4 = 2.0 * Math.Atan(Math.Tan(num2 / 2.0) * Math.Sqrt((1.0 + eccentricity) / (1.0 - eccentricity)));
				double num5 = semiMajor * (1.0 - eccentricity * Math.Cos(num2));
				double num6 = num5 * Math.Sin(num4 + periastron / (180.0 / Math.PI)) * Math.Cos(inclination / (180.0 / Math.PI));
				double num7 = num5 * Math.Cos(num4 + periastron / (180.0 / Math.PI));
				for (PA = Math.Atan2(num6, num7) * (180.0 / Math.PI) + node; PA < 0.0; PA += 360.0)
				{
				}
				while (PA > 360.0)
				{
					PA -= 360.0;
				}
				Sep = Math.Sqrt(num6 * num6 + num7 * num7);
			}
		}
	}
}
