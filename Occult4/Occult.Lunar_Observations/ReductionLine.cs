using System;
using System.Text;

namespace Occult.Lunar_Observations
{
	internal class ReductionLine : IComparable
	{
		internal static int SortField;

		private int seqNumber;

		private int starNumber;

		private int year;

		private int month;

		private int day;

		private int hour;

		private int minute;

		private int second_DecPlaces;

		private int certainty;

		private double second;

		private double o_C;

		private double pA;

		private double L;

		private double B;

		private double aA;

		private double limb;

		private double scale;

		private double p;

		private double d;

		private double residualRate;

		private string starCat = " ";

		private string observer;

		private string doubleStarComponent = " ";

		private string siteCode = " ";

		private string positionSource = "";

		private string eventPhase = " ";

		private string eventLimb = " ";

		private string grazeFlag = " ";

		private string methodTimeRecording = " ";

		private string wdsCode = " ";

		private string doubleCode = " ";

		private bool inGaia;

		public int SeqNumber
		{
			get
			{
				return seqNumber;
			}
			set
			{
				seqNumber = value;
			}
		}

		public string StarCat
		{
			get
			{
				return starCat;
			}
			set
			{
				starCat = value;
			}
		}

		public int StarNumber
		{
			get
			{
				return starNumber;
			}
			set
			{
				starNumber = value;
			}
		}

		public string StarCat_Number => starCat + starNumber;

		public bool InGaia
		{
			get
			{
				return inGaia;
			}
			set
			{
				inGaia = value;
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

		public string SiteCode
		{
			get
			{
				return siteCode;
			}
			set
			{
				siteCode = value;
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

		public string Date => year + " " + Utilities.ShortMonths[month] + " " + day;

		public int Hour
		{
			get
			{
				return hour;
			}
			set
			{
				hour = value;
			}
		}

		public int Minute
		{
			get
			{
				return minute;
			}
			set
			{
				minute = value;
			}
		}

		public double Second
		{
			get
			{
				return second;
			}
			set
			{
				second = value;
			}
		}

		public int Second_DecPlaces
		{
			get
			{
				return second_DecPlaces;
			}
			set
			{
				second_DecPlaces = value;
			}
		}

		public string EventPhase
		{
			get
			{
				return eventPhase;
			}
			set
			{
				eventPhase = value;
			}
		}

		public string EventLimb
		{
			get
			{
				return eventLimb;
			}
			set
			{
				eventLimb = value;
			}
		}

		public string GrazeFlag
		{
			get
			{
				return grazeFlag;
			}
			set
			{
				grazeFlag = value;
			}
		}

		public string MethodTimeRecording
		{
			get
			{
				return methodTimeRecording;
			}
			set
			{
				methodTimeRecording = value;
			}
		}

		public string WDSCode
		{
			get
			{
				return wdsCode;
			}
			set
			{
				wdsCode = value;
			}
		}

		public string DoubleCode
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

		public int Certainty
		{
			get
			{
				return certainty;
			}
			set
			{
				certainty = value;
			}
		}

		public string DoubleStarComponent
		{
			get
			{
				return doubleStarComponent;
			}
			set
			{
				doubleStarComponent = value;
			}
		}

		public double O_C
		{
			get
			{
				return o_C;
			}
			set
			{
				o_C = value;
			}
		}

		public double ResidualRate
		{
			get
			{
				return residualRate;
			}
			set
			{
				residualRate = value;
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

		public double l
		{
			get
			{
				return L;
			}
			set
			{
				L = value;
			}
		}

		public double b
		{
			get
			{
				return B;
			}
			set
			{
				B = value;
			}
		}

		public double AA
		{
			get
			{
				return NormalisedAA();
			}
			set
			{
				aA = value;
			}
		}

		public double Limb
		{
			get
			{
				return limb;
			}
			set
			{
				limb = value;
			}
		}

		public double Scale
		{
			get
			{
				return scale;
			}
			set
			{
				scale = value;
			}
		}

		public double P
		{
			get
			{
				return p;
			}
			set
			{
				p = value;
			}
		}

		public double D
		{
			get
			{
				return d;
			}
			set
			{
				d = value;
			}
		}

		public string PositionSource
		{
			get
			{
				return positionSource;
			}
			set
			{
				positionSource = value;
			}
		}

		public double EventJDTime => Utilities.JD_from_Date(year, month, day) + ((double)hour + (double)minute / 60.0 + second / 3600.0) / 24.0;

		public string EventTime => string.Format("{0,3:F0}", Hour) + string.Format("{0,3:F0}", Minute) + string.Format("{0,5:F1}", Second);

		private double NormalisedAA()
		{
			if (aA < 0.0)
			{
				return aA += 360.0;
			}
			return aA;
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append($"{seqNumber:F0}".PadLeft(6, '0').Substring(3, 3));
			stringBuilder.Append("  " + siteCode.PadLeft(1) + "  " + observer.PadRight(20).Substring(0, 20) + " ");
			stringBuilder.Append(starCat);
			stringBuilder.Append(string.Format("{0,7:F0}", starNumber).Substring(0, 7));
			stringBuilder.Append(WDSCode.PadLeft(1));
			stringBuilder.AppendFormat("{0,5:F0}", year);
			stringBuilder.AppendFormat("{0,3:F0}", month);
			stringBuilder.AppendFormat("{0,3:F0}", day);
			stringBuilder.AppendFormat("{0,3:F0}", hour);
			stringBuilder.AppendFormat("{0,3:F0}", minute);
			if (second_DecPlaces > 1)
			{
				stringBuilder.AppendFormat("{0,6:F2}", second);
			}
			else if (second_DecPlaces == 1)
			{
				stringBuilder.AppendFormat("{0,5:F1} ", second);
			}
			else
			{
				stringBuilder.AppendFormat("{0,3:F0}   ", second);
			}
			stringBuilder.Append(EventPhase.PadLeft(3));
			stringBuilder.Append(EventLimb.PadLeft(1));
			stringBuilder.Append(GrazeFlag.PadLeft(2));
			stringBuilder.Append(MethodTimeRecording.PadLeft(2));
			stringBuilder.AppendFormat("{0,2:F0}", certainty);
			stringBuilder.Append(doubleStarComponent.PadLeft(2).Substring(0, 2));
			stringBuilder.AppendFormat("{0,8:+0;-0}", o_C * 1000.0);
			if (InGaia)
			{
				stringBuilder.Append(" ");
			}
			else
			{
				stringBuilder.Append("§");
			}
			stringBuilder.AppendFormat("{0,8:F2}", o_C / ResidualRate);
			stringBuilder.AppendFormat("{0,7:F2}", limb);
			if (pA > 0.0)
			{
				stringBuilder.AppendFormat("{0,7:F2}", pA);
			}
			else
			{
				stringBuilder.AppendFormat("{0,7:F2}", pA + 360.0);
			}
			stringBuilder.AppendFormat("{0,7:F2}", L);
			stringBuilder.AppendFormat("{0,6:F2}", B);
			if (aA > 0.0)
			{
				stringBuilder.AppendFormat("{0,7:F2}", aA);
			}
			else
			{
				stringBuilder.AppendFormat("{0,7:F2}", aA + 360.0);
			}
			if (p > 0.0)
			{
				stringBuilder.AppendFormat("{0,7:F2}", p);
			}
			else
			{
				stringBuilder.AppendFormat("{0,7:F2}", p + 360.0);
			}
			stringBuilder.AppendFormat("{0,6:F2}", d);
			stringBuilder.AppendFormat("{0,7:F3}", scale);
			stringBuilder.Append("  " + PositionSource.PadRight(5));
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (SeqNumber == ((ReductionLine)other).SeqNumber)
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return SeqNumber.CompareTo(((ReductionLine)other).SeqNumber);
			case 1:
				if (Observer == ((ReductionLine)other).Observer)
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return Observer.CompareTo(((ReductionLine)other).Observer);
			case 2:
				if (Observer.Substring(2) == ((ReductionLine)other).Observer.Substring(2))
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return Observer.Substring(2).CompareTo(((ReductionLine)other).Observer.Substring(2));
			case 3:
				if (StarCat == ((ReductionLine)other).StarCat)
				{
					if (StarNumber == ((ReductionLine)other).StarNumber)
					{
						return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
					}
					return StarNumber.CompareTo(((ReductionLine)other).StarNumber);
				}
				return StarCat.CompareTo(((ReductionLine)other).StarCat);
			case 4:
				return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
			case 5:
				if (o_C == ((ReductionLine)other).o_C)
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return o_C.CompareTo(((ReductionLine)other).o_C);
			case 6:
				if (PA == ((ReductionLine)other).PA)
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return PA.CompareTo(((ReductionLine)other).PA);
			case 7:
				if (AA == ((ReductionLine)other).AA)
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return AA.CompareTo(((ReductionLine)other).AA);
			case 8:
				if (P == ((ReductionLine)other).P)
				{
					return EventJDTime.CompareTo(((ReductionLine)other).EventJDTime);
				}
				return P.CompareTo(((ReductionLine)other).P);
			default:
				return 0;
			}
		}
	}
}
