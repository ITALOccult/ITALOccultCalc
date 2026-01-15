using System;

namespace Occult.Asteroid_Observations
{
	internal class SlowEvents : IComparable
	{
		private double duration;

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

		public double Duration
		{
			get
			{
				return duration;
			}
			set
			{
				duration = value;
			}
		}

		public int CompareTo(object other)
		{
			return -Duration.CompareTo(((SlowEvents)other).Duration);
		}
	}
}
