using System;

namespace Occult
{
	internal class LunarGrazeID : IComparable
	{
		private string date;

		private string starID;

		private string l;

		private string b;

		private int count;

		private static int sortField;

		internal string L
		{
			get
			{
				return l;
			}
			set
			{
				l = value;
			}
		}

		internal string B
		{
			get
			{
				return b;
			}
			set
			{
				b = value;
			}
		}

		internal int Count
		{
			get
			{
				return count;
			}
			set
			{
				count = value;
			}
		}

		internal string Date
		{
			get
			{
				return date;
			}
			set
			{
				date = value;
			}
		}

		internal string StarID
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

		internal static int SortField
		{
			get
			{
				return sortField;
			}
			set
			{
				sortField = value;
			}
		}

		public int CompareTo(object other)
		{
			return sortField switch
			{
				0 => date.CompareTo(((LunarGrazeID)other).Date), 
				1 => starID.CompareTo(((LunarGrazeID)other).StarID), 
				2 => L.CompareTo(((LunarGrazeID)other).l), 
				3 => B.CompareTo(((LunarGrazeID)other).b), 
				4 => ((LunarGrazeID)other).count.CompareTo(Count), 
				_ => 0, 
			};
		}

		public override string ToString()
		{
			return date + "   " + starID + "   " + l + "  " + b + string.Format("{0,4:F0}", count);
		}
	}
}
