using System;

namespace Occult
{
	internal class MoonDistances : IComparable
	{
		private double distance;

		private int satNum;

		internal double SatDistance
		{
			get
			{
				return distance;
			}
			set
			{
				distance = value;
			}
		}

		internal int SatNum
		{
			get
			{
				return satNum;
			}
			set
			{
				satNum = value;
			}
		}

		public int CompareTo(object other)
		{
			return SatDistance.CompareTo(((MoonDistances)other).SatDistance);
		}
	}
}
