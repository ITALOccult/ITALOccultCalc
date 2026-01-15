using System.Text;

namespace Occult.Lunar_Observations
{
	internal class DSolve
	{
		private double t1;

		private double t2;

		private double pA;

		private double cCT;

		private double rV;

		private double limbSlope;

		private int year = 1600;

		private int month = 1;

		private int day = 1;

		private string observer = "";

		private string cA = "";

		private string illum = "";

		private string alt = "";

		private double ll;

		private double bb;

		private double aa;

		private double aa_DegreesPerArcsec = 0.06;

		private double aa_AtEvent;

		private double aa_At2nd;

		private double limb_AtAA;

		private double limb_AtEvent;

		private double limb_At2nd;

		private double moonscale = 1.0;

		private double o_c;

		private bool ischecked = true;

		public double T1
		{
			get
			{
				return t1;
			}
			set
			{
				t1 = value;
			}
		}

		public double T2
		{
			get
			{
				return t2;
			}
			set
			{
				t2 = value;
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

		public double CCT
		{
			get
			{
				return cCT;
			}
			set
			{
				cCT = value;
			}
		}

		public double RV
		{
			get
			{
				return rV;
			}
			set
			{
				rV = value;
			}
		}

		public double LimbSlope
		{
			get
			{
				return limbSlope;
			}
			set
			{
				limbSlope = value;
			}
		}

		public bool IsChecked
		{
			get
			{
				return ischecked;
			}
			set
			{
				ischecked = value;
			}
		}

		public int Year
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

		public int Month
		{
			get
			{
				return month;
			}
			set
			{
				month = value;
			}
		}

		public int Day
		{
			get
			{
				return day;
			}
			set
			{
				day = value;
			}
		}

		public string Observer
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

		public string CA
		{
			get
			{
				return cA;
			}
			set
			{
				cA = value;
			}
		}

		public string Illum
		{
			get
			{
				return illum;
			}
			set
			{
				illum = value;
			}
		}

		public string Alt
		{
			get
			{
				return alt;
			}
			set
			{
				alt = value;
			}
		}

		public double l
		{
			get
			{
				return ll;
			}
			set
			{
				ll = value;
			}
		}

		public double b
		{
			get
			{
				return bb;
			}
			set
			{
				bb = value;
			}
		}

		public double AA
		{
			get
			{
				return aa;
			}
			set
			{
				aa = value;
			}
		}

		public double MoonScale
		{
			get
			{
				return moonscale;
			}
			set
			{
				moonscale = value;
			}
		}

		public double AA_DegreesPerArcsec
		{
			get
			{
				return aa_DegreesPerArcsec;
			}
			set
			{
				aa_DegreesPerArcsec = value;
			}
		}

		public double AA_AtEvent
		{
			get
			{
				return aa_AtEvent;
			}
			set
			{
				aa_AtEvent = value;
			}
		}

		public double AA_At2nd
		{
			get
			{
				return aa_At2nd;
			}
			set
			{
				aa_At2nd = value;
			}
		}

		public double Limb_AtAA
		{
			get
			{
				return limb_AtAA;
			}
			set
			{
				limb_AtAA = value;
			}
		}

		public double Limb_AtEvent
		{
			get
			{
				return limb_AtEvent;
			}
			set
			{
				limb_AtEvent = value;
			}
		}

		public double Limb_At2nd
		{
			get
			{
				return limb_At2nd;
			}
			set
			{
				limb_At2nd = value;
			}
		}

		public double O_C
		{
			get
			{
				return o_c;
			}
			set
			{
				o_c = value;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,4:F0}", year);
			stringBuilder.AppendFormat(" {0,2:f0}", month);
			stringBuilder.AppendFormat(" {0,2:f0}  ", day);
			stringBuilder.Append(observer.Trim().PadRight(16).Substring(0, 16));
			stringBuilder.AppendFormat("{0,6:F2}  ", t1);
			stringBuilder.AppendFormat("{0,6:F2}  ", t2);
			stringBuilder.AppendFormat("{0,6:F2}  ", t2 - t1);
			stringBuilder.AppendFormat("{0,6:F2}  ", pA);
			stringBuilder.AppendFormat("{0,5:F4} ", rV);
			stringBuilder.AppendFormat("{0,7:F2}  ", cCT);
			stringBuilder.AppendFormat("{0,6:F2}  ", limbSlope);
			if (ischecked)
			{
				stringBuilder.Append("1");
			}
			else
			{
				stringBuilder.Append(" ");
			}
			stringBuilder.AppendFormat("{0,7:F2}", ll);
			stringBuilder.AppendFormat("{0,7:F2}", bb);
			stringBuilder.AppendFormat("{0,8:F2}", aa);
			stringBuilder.AppendFormat("{0,6:F2}", limb_AtAA);
			stringBuilder.AppendFormat("{0,6:F3}", moonscale);
			stringBuilder.AppendFormat("{0,6:F3}", aa_DegreesPerArcsec);
			stringBuilder.AppendFormat("{0,8:F2}", aa_AtEvent);
			stringBuilder.AppendFormat("{0,6:F2}", limb_AtEvent);
			stringBuilder.AppendFormat("{0,8:F2}", aa_At2nd);
			stringBuilder.AppendFormat("{0,6:F2}", limb_At2nd);
			stringBuilder.Append(cA.PadLeft(5).Substring(0, 5));
			stringBuilder.Append(illum.PadLeft(4).Substring(0, 4));
			stringBuilder.Append(alt.PadLeft(4).Substring(0, 4));
			stringBuilder.AppendFormat("{0,7:F3}", o_c);
			return stringBuilder.ToString();
		}
	}
}
