using System;

namespace Occult.Star_Catalogues
{
	internal class SAO : IComparable
	{
		private int number;

		private float mag;

		private float separation;

		public int Number
		{
			get
			{
				return number;
			}
			set
			{
				number = value;
			}
		}

		public float Mag
		{
			get
			{
				return mag;
			}
			set
			{
				mag = value;
			}
		}

		public float Separation
		{
			get
			{
				return separation;
			}
			set
			{
				separation = value;
			}
		}

		public int CompareTo(object other)
		{
			return Separation.CompareTo(((SAO)other).Separation);
		}
	}
}
