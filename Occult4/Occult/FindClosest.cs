using System;

namespace Occult
{
	internal class FindClosest : IComparable
	{
		private double separation2000 = 1.0;

		private double magDiff;

		private double separation2016;

		private int starEntry;

		internal static bool SortBy2000 = true;

		internal double Separation2000
		{
			get
			{
				return separation2000;
			}
			set
			{
				separation2000 = value;
			}
		}

		internal double Separation2016
		{
			get
			{
				return separation2016;
			}
			set
			{
				separation2016 = value;
			}
		}

		internal double Separation
		{
			get
			{
				if (SortBy2000)
				{
					return separation2000;
				}
				return separation2016;
			}
		}

		internal double MagDiff
		{
			get
			{
				return magDiff;
			}
			set
			{
				magDiff = value;
			}
		}

		internal int StarEntry
		{
			get
			{
				return starEntry;
			}
			set
			{
				starEntry = value;
			}
		}

		public int CompareTo(object other)
		{
			if (SortBy2000)
			{
				return Separation.CompareTo(((FindClosest)other).Separation);
			}
			return Separation2016.CompareTo(((FindClosest)other).Separation2016);
		}
	}
}
