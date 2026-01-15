using System;
using System.Text;
using Occult;

namespace LightCurves
{
	internal class Embargoed : IComparable
	{
		private string asteroidNumber = "";

		private int yearStart = DateTime.UtcNow.Year;

		private int monthStart = DateTime.UtcNow.Month;

		private int yearEnd = DateTime.UtcNow.AddMonths(12).Year;

		private int monthEnd = DateTime.UtcNow.AddMonths(12).Month;

		private string requestor = "";

		private string contactInfo = "";

		internal static string Header_File = "Asteroid,StartYear,StartMonth,EndYear,EndMonth,Requestor,Contact";

		internal static string Header_View = "Asteroid    Year,Month to Year,Month   Requestor       Contact";

		internal string AsteroidNumber
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

		internal int YearStart
		{
			get
			{
				return yearStart;
			}
			set
			{
				yearStart = value;
			}
		}

		internal int MonthStart
		{
			get
			{
				return monthStart;
			}
			set
			{
				monthStart = value;
			}
		}

		internal int YearEnd
		{
			get
			{
				return yearEnd;
			}
			set
			{
				yearEnd = value;
			}
		}

		internal int MonthEnd
		{
			get
			{
				return monthEnd;
			}
			set
			{
				monthEnd = value;
			}
		}

		internal string Requestor
		{
			get
			{
				return requestor;
			}
			set
			{
				requestor = value;
			}
		}

		internal string ContactInfo
		{
			get
			{
				return contactInfo;
			}
			set
			{
				contactInfo = value;
			}
		}

		internal double JDstartEmbargo => Utilities.JD_from_Date(YearStart, MonthStart, 1.0);

		internal double JDendEmbargo => Utilities.JD_from_Date(YearEnd, MonthEnd, 31.0);

		internal string DisplayLine
		{
			get
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(AsteroidNumber.PadLeft(8));
				stringBuilder.AppendFormat("    {0,4} {1,2}    to {2,4} {3,2}      ", YearStart, MonthStart, YearEnd, MonthEnd);
				stringBuilder.Append(Requestor.PadRight(17).Substring(0, 16));
				stringBuilder.Append(ContactInfo);
				return stringBuilder.ToString();
			}
		}

		internal void ReadLine(string InLine)
		{
			string[] array = InLine.Trim().Split(new char[1] { ',' });
			AsteroidNumber = array[0];
			int.TryParse(array[1], out yearStart);
			int.TryParse(array[2], out monthStart);
			int.TryParse(array[3], out yearEnd);
			int.TryParse(array[4], out monthEnd);
			Requestor = array[5];
			ContactInfo = array[6];
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(asteroidNumber.ToString() + ",");
			stringBuilder.Append(YearStart + ",");
			stringBuilder.Append(MonthStart + ",");
			stringBuilder.Append(YearEnd + ",");
			stringBuilder.Append(MonthEnd + ",");
			stringBuilder.Append(Requestor.ToString() + ",");
			stringBuilder.Append(ContactInfo.ToString());
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			return AsteroidNumber.CompareTo(((Embargoed)other).AsteroidNumber);
		}
	}
}
