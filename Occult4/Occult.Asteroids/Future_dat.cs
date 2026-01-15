using System;

namespace Occult.Asteroids
{
	internal class Future_dat : IComparable
	{
		private string date;

		private string star;

		private int asteroidNumber;

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

		public string Date
		{
			get
			{
				return date;
			}
			set
			{
				date = value;
			}
		}

		public string Star
		{
			get
			{
				return star;
			}
			set
			{
				star = value;
			}
		}

		public int CompareTo(object other)
		{
			if (AsteroidNumber == ((Future_dat)other).AsteroidNumber)
			{
				return Date.CompareTo(((Future_dat)other).Date);
			}
			return AsteroidNumber.CompareTo(((Future_dat)other).AsteroidNumber);
		}
	}
}
