using System;

namespace Occult.Asteroids
{
	internal class RIO_Updates : IComparable
	{
		public static int SortField;

		private string id;

		private double jdEvent;

		private double mjdUpdate;

		private int index;

		private bool valid = true;

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

		internal bool Valid
		{
			get
			{
				return valid;
			}
			set
			{
				valid = value;
			}
		}

		internal int Index
		{
			get
			{
				return index;
			}
			set
			{
				index = value;
			}
		}

		internal double JDevent
		{
			get
			{
				return jdEvent;
			}
			set
			{
				jdEvent = value;
			}
		}

		internal double MJD_update
		{
			get
			{
				return mjdUpdate;
			}
			set
			{
				mjdUpdate = value;
			}
		}

		internal void ReadLine(string InLine, int Index)
		{
			int result = 2000;
			int result2 = 1;
			double result3 = 1.0;
			double result4 = 0.0;
			int num = 0;
			index = Index;
			if (!int.TryParse(InLine.Substring(0, 4), out result))
			{
				result = 2000;
			}
			if (!int.TryParse(InLine.Substring(5, 2), out result2))
			{
				result2 = 1;
			}
			if (!double.TryParse(InLine.Substring(8, 2), out result3))
			{
				result3 = 1.0;
			}
			mjdUpdate = Utilities.JD_from_Date(result, result2, result3 + result4 / 24.0) - 2400000.0;
			num = InLine.IndexOf("-", 12) - 5;
			if (!int.TryParse(InLine.Substring(num + 1, 4), out result))
			{
				result = 2000;
			}
			if (!int.TryParse(InLine.Substring(num + 6, 2), out result2))
			{
				result2 = 1;
			}
			if (!double.TryParse(InLine.Substring(num + 9, 2), out result3))
			{
				result3 = 1.0;
			}
			if (!double.TryParse(InLine.Substring(num + 12), out result4))
			{
				result4 = 0.0;
			}
			jdEvent = Utilities.JD_from_Date(result, result2, result3 + result4 / 24.0);
			id = InLine.Substring(17, num - 17).Trim();
			num = id.IndexOf("/");
			if (num > 0)
			{
				id = id.Substring(0, num);
			}
		}

		public int CompareTo(object other)
		{
			if (SortField == 0)
			{
				return Index.CompareTo(((RIO_Updates)other).Index);
			}
			if (ID == ((RIO_Updates)other).ID)
			{
				if (Math.Abs(JDevent - ((RIO_Updates)other).JDevent) - 0.1 > 0.0)
				{
					return MJD_update.CompareTo(((RIO_Updates)other).MJD_update);
				}
				return JDevent.CompareTo(((RIO_Updates)other).JDevent);
			}
			return ((RIO_Updates)other).ID.CompareTo(ID);
		}
	}
}
