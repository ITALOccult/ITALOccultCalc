using System;

namespace Occult.Mapping
{
	internal class Points : IComparable
	{
		private double aa;

		private double h;

		private double d;

		private bool valid = true;

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

		public double H
		{
			get
			{
				return h;
			}
			set
			{
				h = value;
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

		public bool Valid
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

		public int CompareTo(object other)
		{
			if (AA.CompareTo(((Points)other).AA) == 0)
			{
				return D.CompareTo(((Points)other).D);
			}
			return AA.CompareTo(((Points)other).AA);
		}
	}
}
