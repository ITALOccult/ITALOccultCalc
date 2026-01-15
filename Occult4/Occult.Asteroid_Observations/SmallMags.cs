using System;

namespace Occult.Asteroid_Observations
{
	internal class SmallMags : IComparable
	{
		private double drop;

		private string label;

		public string Label
		{
			get
			{
				return label;
			}
			set
			{
				label = value;
			}
		}

		public double Drop
		{
			get
			{
				return drop;
			}
			set
			{
				drop = value;
			}
		}

		public int CompareTo(object other)
		{
			return Drop.CompareTo(((SmallMags)other).Drop);
		}
	}
}
