using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class AsteroidDoubleStars : IComparable
	{
		internal static string WDS_SummaryHeader = "WDS_ID    Ref          Yr1  Yr2     # PA1 PA2  Sep1  Sep2  Mv1   Mv2                                            WDS_PreciseID";

		internal static string WDS_SubmissionHeader = "WDS_ID     Comp  Date        PA       e_PA     Sep      e_Sep     M1    e_M1    M2    e_M2   WL  FW     Ap    N Ref      Me  DE";

		private const double Radian = 180.0 / Math.PI;

		private string asteroidNumber = "";

		private string asteroidID = "";

		private string starID = "";

		private string companionID = "";

		private string jDSO_submitDate = "";

		private string jDSO_Vol_Num_Pg = "";

		private string observers;

		private string reference = "";

		private string method = "O";

		private string dataEntryNote = "";

		private double sep;

		private double pA;

		private double rUWE = -1.0;

		private double starDia_mas;

		private double mv1 = 25.0;

		private double mv2 = 25.0;

		private double mv1_err = 0.2;

		private double mv2_err = 0.2;

		private double hour;

		private double sdev_Sep;

		private double sdev_PA;

		private double rA2000;

		private double dec2000;

		private int year = 1900;

		private int month = 1;

		private int day = 1;

		private int numberNights = 1;

		public static int SortField = 2;

		private int numberofSolutions;

		private bool singleComponentOnly;

		private bool isMatchedToWDS;

		public string AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
			}
		}

		public string AsteroidID
		{
			get
			{
				return asteroidID;
			}
			set
			{
				asteroidID = value;
			}
		}

		public string StarID
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

		public string CompanionID
		{
			get
			{
				return companionID;
			}
			set
			{
				companionID = value;
			}
		}

		public string JDSO_SubmitDate
		{
			get
			{
				return jDSO_submitDate;
			}
			set
			{
				jDSO_submitDate = value;
			}
		}

		public string JDSO_Vol_Num_Pg
		{
			get
			{
				return jDSO_Vol_Num_Pg;
			}
			set
			{
				jDSO_Vol_Num_Pg = value;
			}
		}

		public double RUWE
		{
			get
			{
				return rUWE;
			}
			set
			{
				rUWE = value;
			}
		}

		public double StarDia_mas
		{
			get
			{
				return starDia_mas;
			}
			set
			{
				starDia_mas = value;
			}
		}

		public double RA2000
		{
			get
			{
				return rA2000;
			}
			set
			{
				rA2000 = value;
			}
		}

		public double Dec2000
		{
			get
			{
				return dec2000;
			}
			set
			{
				dec2000 = value;
			}
		}

		public string WDS_id
		{
			get
			{
				string text = Utilities.DEGtoDMS(RA2000 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: true, IncludeLeadingZeros: true, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: true).Replace(" ", "").Replace(".", "");
				string text2 = Utilities.DEGtoDMS(Dec2000 * (180.0 / Math.PI), 3, 0, MinutesOnly: true, IncludeLeadingZeros: true, IncludePlusSymbol: true, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: true).Replace(" ", "");
				return text + text2;
			}
		}

		public string WDS_LongID
		{
			get
			{
				string text = Utilities.DEGtoDMS(RA2000 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: true).Replace(" ", "");
				string text2 = Utilities.DEGtoDMS(Dec2000 * (180.0 / Math.PI), 3, 1, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: true).Replace(" ", "");
				return text + " " + text2;
			}
		}

		public bool IsMatchedToWDS
		{
			get
			{
				return isMatchedToWDS;
			}
			set
			{
				isMatchedToWDS = value;
			}
		}

		public bool SingleComponentOnly
		{
			get
			{
				return singleComponentOnly;
			}
			set
			{
				singleComponentOnly = value;
			}
		}

		public double Mv1
		{
			get
			{
				return mv1;
			}
			set
			{
				mv1 = value;
			}
		}

		public double Mv2
		{
			get
			{
				return mv2;
			}
			set
			{
				mv2 = value;
			}
		}

		public double Mv1_err
		{
			get
			{
				return mv1_err;
			}
			set
			{
				mv1_err = value;
			}
		}

		public double Mv2_err
		{
			get
			{
				return mv2_err;
			}
			set
			{
				mv2_err = value;
			}
		}

		public double Sep_mas
		{
			get
			{
				if ((sep < 0.0) & (sep > -0.3))
				{
					return -0.3;
				}
				return sep;
			}
			set
			{
				sep = value;
			}
		}

		public double PA_deg
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

		public double Sdev_Sep_Star_mas
		{
			get
			{
				return sdev_Sep;
			}
			set
			{
				sdev_Sep = value;
			}
		}

		public double Sdev_PA_Star_deg
		{
			get
			{
				return sdev_PA;
			}
			set
			{
				sdev_PA = value;
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

		public double Hour
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

		public double DateForSort => (double)(year * 10000 + month * 100 + day) + Hour / 24.0;

		public int NumberNights
		{
			get
			{
				return numberNights;
			}
			set
			{
				numberNights = value;
			}
		}

		public int SolutionNumber
		{
			get
			{
				return numberofSolutions;
			}
			set
			{
				numberofSolutions = value;
			}
		}

		public string Observers
		{
			get
			{
				return observers;
			}
			set
			{
				observers = value;
			}
		}

		public string Star_Date => StarID.Replace("-coords ", "").PadRight(19) + string.Format("  {0,4:F0} ", year) + Utilities.ShortMonths[month] + string.Format("{0,3:F0}", day);

		internal string Reference
		{
			get
			{
				return reference;
			}
			set
			{
				reference = value;
			}
		}

		internal string Method
		{
			get
			{
				return method;
			}
			set
			{
				method = value;
			}
		}

		internal string DataEntryNote
		{
			get
			{
				return dataEntryNote;
			}
			set
			{
				dataEntryNote = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (AsteroidID.CompareTo(((AsteroidDoubleStars)other).AsteroidID) == 0)
				{
					if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
					{
						return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
					}
					return DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort);
				}
				return AsteroidID.CompareTo(((AsteroidDoubleStars)other).AsteroidID);
			case 1:
				if (AsteroidNumber.CompareTo(((AsteroidDoubleStars)other).AsteroidNumber) == 0)
				{
					if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
					{
						return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
					}
					return DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort);
				}
				return AsteroidNumber.CompareTo(((AsteroidDoubleStars)other).AsteroidNumber);
			case 2:
				if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
				{
					return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
				}
				return -DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort);
			case 3:
				if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
				{
					return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
				}
				return DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort);
			case 4:
				if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
				{
					return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
				}
				return StarID.CompareTo(((AsteroidDoubleStars)other).StarID);
			case 5:
				if (RA2000.CompareTo(((AsteroidDoubleStars)other).RA2000) == 0)
				{
					if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
					{
						return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
					}
					return DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort);
				}
				return RA2000.CompareTo(((AsteroidDoubleStars)other).RA2000);
			case 6:
				if (RUWE.CompareTo(((AsteroidDoubleStars)other).RUWE) == 0)
				{
					if (DateForSort.CompareTo(((AsteroidDoubleStars)other).DateForSort) == 0)
					{
						return SolutionNumber.CompareTo(((AsteroidDoubleStars)other).SolutionNumber);
					}
					return StarID.CompareTo(((AsteroidDoubleStars)other).StarID);
				}
				return RUWE.CompareTo(((AsteroidDoubleStars)other).RUWE);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(WDS_id + CompanionID.Trim().PadRight(6));
			stringBuilder.Append(StarID.Replace("-coords ", "").PadRight(19));
			stringBuilder.AppendFormat("{0,5:F1}", Mv1);
			stringBuilder.AppendFormat("{0,5:F1}", Mv2);
			stringBuilder.Append("   #" + SolutionNumber + " ");
			if ((Sep_mas == 0.0) | (Sep_mas < -999.0))
			{
				stringBuilder.Append("".PadRight(5) + "?" + "".PadRight(7));
			}
			else
			{
				string value = string.Format("{0,7:F1}", Sep_mas).Replace("-", ">");
				stringBuilder.Append(value);
				if ((Sdev_Sep_Star_mas > 0.0) & (Sdev_Sep_Star_mas < 10.0))
				{
					stringBuilder.AppendFormat(" ±{0,4:F1}", Sdev_Sep_Star_mas);
				}
				else
				{
					stringBuilder.Append("".PadRight(6));
				}
			}
			if (PA_deg < 0.0)
			{
				stringBuilder.Append("".PadRight(7) + "?" + "".PadRight(5));
			}
			else
			{
				if (sep < 0.0)
				{
					PA_deg = (double)Convert.ToInt32(PA_deg / 10.0) * 10.0;
				}
				stringBuilder.AppendFormat("{0,9:F0}", PA_deg);
				if ((Sdev_PA_Star_deg > 0.0) & (Sdev_PA_Star_deg < 50.0))
				{
					stringBuilder.AppendFormat(" ±{0,2:f0}", Sdev_PA_Star_deg);
				}
				else
				{
					stringBuilder.Append("".PadRight(4));
				}
			}
			if (RUWE > 0.0)
			{
				stringBuilder.AppendFormat("{0,7:f2}", RUWE);
			}
			else
			{
				stringBuilder.Append("       ");
			}
			if (StarDia_mas > 0.05)
			{
				stringBuilder.AppendFormat("{0,5:f1}", StarDia_mas);
			}
			else
			{
				stringBuilder.Append("     ");
			}
			stringBuilder.AppendFormat("  {0,4:F0} ", year);
			stringBuilder.Append(Utilities.ShortMonths[month]);
			stringBuilder.AppendFormat("{0,3:F0}", day);
			stringBuilder.Append("  " + asteroidNumber + " " + asteroidID.PadRight(18));
			if (IsMatchedToWDS)
			{
				stringBuilder.Append(" WDS   ");
			}
			else
			{
				stringBuilder.Append(" ---   ");
			}
			if (JDSO_Vol_Num_Pg.Length > 0)
			{
				stringBuilder.Append("JDSO " + JDSO_Vol_Num_Pg);
			}
			else if (JDSO_SubmitDate.Length > 0)
			{
				stringBuilder.Append("Submitted " + JDSO_SubmitDate);
			}
			else
			{
				stringBuilder.Append("-");
			}
			return stringBuilder.ToString();
		}

		public string WDS_SummaryLine()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(WDS_id + " ".PadRight(7));
			stringBuilder.Append(CompanionID.Trim().PadRight(6));
			stringBuilder.AppendFormat("{0,4} {0,4}", Year);
			stringBuilder.AppendFormat("{0,5:F0}", NumberNights);
			if (SingleComponentOnly)
			{
				stringBuilder.AppendFormat(" {0,3} {0,3}", 10 * ((int)PA_deg / 10));
			}
			else
			{
				stringBuilder.AppendFormat(" {0,3} {0,3}", (int)PA_deg);
			}
			if (sep < 100.0)
			{
				stringBuilder.Append("   0.1   0.1");
			}
			else
			{
				stringBuilder.AppendFormat("{0,6:f1}{0,6:f1}", sep / 1000.0);
			}
			stringBuilder.AppendFormat("{0,5:f1}{1,6:f1}", Mv1, Mv2);
			stringBuilder.Append("".PadRight(44) + WDS_LongID);
			if (IsMatchedToWDS)
			{
				stringBuilder.Append(" WDS ");
			}
			if (JDSO_Vol_Num_Pg.Length > 0)
			{
				stringBuilder.Append(" JDSO");
			}
			return stringBuilder.ToString();
		}

		public string WDS_SubmissionLine()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(WDS_id + " ");
			stringBuilder.Append(CompanionID.Trim().PadRight(6));
			stringBuilder.AppendFormat("{0,8:f3}   ", (double)Year + (Utilities.JD_from_Date(Year, Month, (double)Day + Hour / 24.0) - Utilities.JD_from_Date(Year, 1, 1.0)) / 365.25);
			if (SingleComponentOnly)
			{
				int num = 10 * ((int)PA_deg / 10);
				stringBuilder.AppendFormat("V{0,3:f0}.   ", num);
			}
			else
			{
				stringBuilder.AppendFormat(" {0,3:f0}.   ", PA_deg);
			}
			if ((Sdev_PA_Star_deg <= 1.0) | (Sdev_PA_Star_deg > 45.0) | SingleComponentOnly)
			{
				stringBuilder.Append("   .    ");
			}
			else
			{
				stringBuilder.Append(string.Format("{0,3:f0}.", Sdev_PA_Star_deg).PadRight(8));
			}
			if (SingleComponentOnly)
			{
				stringBuilder.Append(">");
			}
			else
			{
				stringBuilder.Append(" ");
			}
			stringBuilder.AppendFormat("{0,8:f4}  ", Math.Abs(Sep_mas) / 1000.0);
			if ((Sdev_Sep_Star_mas / Sep_mas <= 0.05) | (Sdev_Sep_Star_mas / Sep_mas > 0.5) | SingleComponentOnly)
			{
				stringBuilder.Append("  .       ");
			}
			else
			{
				stringBuilder.Append(string.Format(" {0,3:f4}   ", Sdev_Sep_Star_mas / 1000.0).PadRight(8));
			}
			stringBuilder.AppendFormat("{0,4:f1}  ", Mv1);
			stringBuilder.AppendFormat("{0,4:f1}   ", Mv1_err);
			stringBuilder.AppendFormat("{0,5:f1}  ", Mv2);
			stringBuilder.AppendFormat("{0,4:f1}   ", Mv2_err);
			stringBuilder.Append("             . ");
			stringBuilder.AppendFormat("{0,4:F0} ", NumberNights);
			stringBuilder.Append(Reference.PadRight(8).Substring(0, 8) + " ");
			stringBuilder.Append(Method);
			stringBuilder.Append(DataEntryNote.PadRight(2));
			if (IsMatchedToWDS)
			{
				stringBuilder.Append("WDS ");
			}
			else
			{
				stringBuilder.Append("    ");
			}
			if (JDSO_Vol_Num_Pg.Length > 0)
			{
				stringBuilder.Append("JDSO");
			}
			return stringBuilder.ToString();
		}

		public string VizieR()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.DEGtoDMS(RA2000 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false) + " ");
			stringBuilder.Append(Utilities.DEGtoDMS(Dec2000 * (180.0 / Math.PI), 3, 0, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true) + " ");
			stringBuilder.Append(StarID.PadRight(20));
			stringBuilder.AppendFormat(" {0,1:f0}", SolutionNumber);
			stringBuilder.AppendFormat(" {0,6:F1}", sep);
			if ((Sdev_Sep_Star_mas > 0.0) & (Sdev_Sep_Star_mas < 10.0))
			{
				stringBuilder.AppendFormat(" {0,4:F1}", Sdev_Sep_Star_mas);
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			stringBuilder.AppendFormat(" {0,5:F1}", PA_deg);
			if ((Sdev_PA_Star_deg > 0.0) & (Sdev_PA_Star_deg < 50.0))
			{
				stringBuilder.AppendFormat(" {0,4:F1}", Sdev_PA_Star_deg);
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			stringBuilder.AppendFormat("  {0,4:F0} {1,2:f0} {2,2:f0}", Year, Month, Day);
			stringBuilder.Append(asteroidNumber.Trim().PadLeft(8) + " " + asteroidID.PadRight(17));
			return stringBuilder.ToString();
		}

		public string LatexTable()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.DEGtoDMS(RA2000 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false) + "&");
			stringBuilder.Append(Utilities.DEGtoDMS(Dec2000 * (180.0 / Math.PI), 3, 0, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true) + "&");
			stringBuilder.Append(StarID.PadRight(20));
			stringBuilder.AppendFormat("&{0,1:f0}", SolutionNumber);
			stringBuilder.AppendFormat("&{0,6:F1}", sep);
			if ((Sdev_Sep_Star_mas > 0.0) & (Sdev_Sep_Star_mas < 10.0))
			{
				stringBuilder.AppendFormat("&{0,4:F1}", Sdev_Sep_Star_mas);
			}
			else
			{
				stringBuilder.Append("&".PadRight(5));
			}
			stringBuilder.AppendFormat("&{0,5:F1}", PA_deg);
			if ((Sdev_PA_Star_deg > 0.0) & (Sdev_PA_Star_deg < 50.0))
			{
				stringBuilder.AppendFormat("&{0,4:F1}", Sdev_PA_Star_deg);
			}
			else
			{
				stringBuilder.Append("&".PadRight(5));
			}
			stringBuilder.AppendFormat("  &{0,4:F0} {1,2:f0} {2,2:f0}&", Year, Month, Day);
			stringBuilder.Append(asteroidNumber.Trim().PadLeft(8) + "& " + asteroidID.PadRight(17));
			return stringBuilder.ToString();
		}
	}
}
