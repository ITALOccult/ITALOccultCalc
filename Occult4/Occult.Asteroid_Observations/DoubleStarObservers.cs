using System;

namespace Occult.Asteroid_Observations
{
	internal class DoubleStarObservers : IComparable
	{
		private string observer = "";

		public string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}

		public string Observer2
		{
			get
			{
				if (observer.IndexOf(" ") < 2)
				{
					return "   " + observer;
				}
				return observer;
			}
		}

		public int CompareTo(object other)
		{
			return Observer2.Substring(2).CompareTo(((DoubleStarObservers)other).Observer2.Substring(2));
		}
	}
}
