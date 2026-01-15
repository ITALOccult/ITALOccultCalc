using System;

namespace Occult.Asteroid_Observations
{
	internal class AsteroidNumbers : IComparable
	{
		private string asteroidName;

		private int asteroidNumber;

		private int eventCount;

		internal static int Sort;

		public string AsteroidName
		{
			get
			{
				return asteroidName;
			}
			set
			{
				asteroidName = value;
			}
		}

		public int AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
			}
		}

		public int EventCount
		{
			get
			{
				return eventCount;
			}
			set
			{
				eventCount = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Sort == 0)
			{
				return AsteroidNumber.CompareTo(((AsteroidNumbers)other).AsteroidNumber);
			}
			if (Sort == 1)
			{
				if (EventCount == ((AsteroidNumbers)other).EventCount)
				{
					return AsteroidNumber.CompareTo(((AsteroidNumbers)other).AsteroidNumber);
				}
				return ((AsteroidNumbers)other).EventCount.CompareTo(EventCount);
			}
			return EventCount.CompareTo(((AsteroidNumbers)other).EventCount);
		}
	}
}
