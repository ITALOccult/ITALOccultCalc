using System;

namespace Occult.MPC_PDS
{
	internal class SubmissionID : IComparable
	{
		private int year;

		private int month;

		private int statusValue;

		private double day;

		private string astNum = "";

		private string id = "";

		private string provid = "";

		private string status = "";

		private static string[] StatCodes = new string[3] { "  ", "  ✔", "  ✘" };

		internal string AstNum
		{
			get
			{
				return astNum.Trim().PadLeft(7);
			}
			set
			{
				Utilities.AsteroidNumber_Unpack(value, out var _, out astNum);
			}
		}

		internal int Year
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

		internal int Month
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

		internal double Day
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

		internal string ID
		{
			get
			{
				return id;
			}
			set
			{
				id = value;
			}
		}

		internal string ProvID
		{
			get
			{
				return provid;
			}
			set
			{
				provid = value;
			}
		}

		internal int StatusValue_None_Good_Bad
		{
			get
			{
				return statusValue;
			}
			set
			{
				statusValue = value;
			}
		}

		internal string Status => StatCodes[statusValue];

		public int CompareTo(object other)
		{
			if (((SubmissionID)other).Year == Year)
			{
				if (((SubmissionID)other).Month == Month)
				{
					return -((SubmissionID)other).Day.CompareTo(Day);
				}
				return -((SubmissionID)other).Month.CompareTo(Month);
			}
			return -((SubmissionID)other).Year.CompareTo(Year);
		}

		public override string ToString()
		{
			return string.Format("{0,6}, {1,10}, {2,4} {3,2} {4,9:f6}, ", AstNum, ProvID, Year, Month, Day) + ID + Status;
		}

		internal bool Decode(string InLine)
		{
			_ = new string[6] { "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" };
			string[] array = new string[8] { "", "", "", "", "", "", "", "" };
			if (!InLine.Contains("->"))
			{
				return false;
			}
			array[0] = InLine.Substring(0, 5);
			array[1] = InLine.Substring(5, 8).Trim();
			array[2] = InLine.Substring(15, 4);
			array[3] = InLine.Substring(20, 2);
			array[4] = InLine.Substring(23, 9);
			array[5] = InLine.Substring(36);
			year = int.Parse(array[2]);
			month = int.Parse(array[3]);
			day = double.Parse(array[4]);
			id = array[5].Trim();
			if (array[0].Trim().Length < 5)
			{
				AstNum = "";
			}
			else if (array[0].Substring(array[0].Length - 1) == "S")
			{
				string value = array[0].Substring(0, 1);
				astNum = "P" + " mVEMJSUNP".IndexOf(value) + "M" + array[0].Substring(2, 2);
			}
			else
			{
				Utilities.AsteroidNumber_Unpack(array[0], out var _, out astNum);
			}
			if (Utilities.AsteroidProvisionalDesignation_Unpack(array[1], out var UnpackedID))
			{
				ProvID = UnpackedID;
			}
			else
			{
				ProvID = "";
			}
			StatusValue_None_Good_Bad = 0;
			return true;
		}

		internal bool DecodeBackcapture(string InLine)
		{
			_ = new string[6] { "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" };
			if (InLine.Contains("obs80") | (InLine.Trim().Length == 0))
			{
				return false;
			}
			string[] array = InLine.Split(new char[1] { ',' });
			string text = array[0].Substring(0, 5).Trim();
			if (text.Substring(text.Length - 1) == "S")
			{
				string value = array[0].Substring(0, 1);
				astNum = "P" + " mVEMJSUNP".IndexOf(value) + "M" + array[0].Substring(2, 2);
			}
			else
			{
				Utilities.AsteroidNumber_Unpack(text, out var _, out astNum);
			}
			year = int.Parse(array[3].Substring(0, 4));
			month = int.Parse(array[3].Substring(5, 2));
			int num = int.Parse(array[3].Substring(8, 2));
			int num2 = int.Parse(array[3].Substring(11, 2));
			int num3 = int.Parse(array[3].Substring(14, 2));
			day = (double)num + (double)num2 / 24.0 + (double)num3 / 1440.0;
			id = array[1].Trim();
			StatusValue_None_Good_Bad = 0;
			return true;
		}
	}
}
