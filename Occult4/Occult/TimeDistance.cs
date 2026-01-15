using System;

namespace Occult
{
	internal class TimeDistance : IComparable
	{
		private double t;

		private double d;

		internal double Distance
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

		internal double T
		{
			get
			{
				return t;
			}
			set
			{
				t = value;
			}
		}

		public int CompareTo(object other)
		{
			return Distance.CompareTo(((TimeDistance)other).Distance);
		}
	}
}
