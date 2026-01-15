using System;

namespace Shapes
{
	internal class DamitModelIndex : IComparable
	{
		private int damit_number;

		private int asteroid_number;

		internal int Asteroid_number
		{
			get
			{
				return asteroid_number;
			}
			set
			{
				asteroid_number = value;
			}
		}

		internal int Damit_number
		{
			get
			{
				return damit_number;
			}
			set
			{
				damit_number = value;
			}
		}

		public int CompareTo(object other)
		{
			return Damit_number.CompareTo(((DamitModelIndex)other).Damit_number);
		}
	}
}
