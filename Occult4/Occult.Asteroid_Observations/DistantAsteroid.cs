using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class DistantAsteroid : IComparable
	{
		private string asteroidNumber;

		private string asteroidID;

		private string date;

		private string astclass;

		private double sortDate;

		private double astNum;

		public static int SortField = 3;

		public string AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
				double.TryParse(value, out astNum);
			}
		}

		public double AstNum => astNum;

		public string AsteroidID
		{
			get
			{
				return asteroidID;
			}
			set
			{
				asteroidID = value;
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

		public string Class
		{
			get
			{
				return astclass;
			}
			set
			{
				astclass = value;
			}
		}

		public double SortDate
		{
			get
			{
				return sortDate;
			}
			set
			{
				sortDate = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (AsteroidID.CompareTo(((DistantAsteroid)other).AsteroidID) == 0)
				{
					return SortDate.CompareTo(((DistantAsteroid)other).SortDate);
				}
				return AsteroidID.CompareTo(((DistantAsteroid)other).AsteroidID);
			case 1:
				if (AsteroidNumber.CompareTo(((DistantAsteroid)other).AsteroidNumber) == 0)
				{
					return SortDate.CompareTo(((DistantAsteroid)other).SortDate);
				}
				if (AsteroidNumber.Substring(1) == "P")
				{
					return AsteroidNumber.CompareTo(((DistantAsteroid)other).AsteroidNumber);
				}
				return AstNum.CompareTo(((DistantAsteroid)other).AstNum);
			case 2:
				return SortDate.CompareTo(((DistantAsteroid)other).SortDate);
			case 3:
				if (Class.CompareTo(((DistantAsteroid)other).Class) == 0)
				{
					return AsteroidNumber.CompareTo(((DistantAsteroid)other).AsteroidNumber);
				}
				return Class.CompareTo(((DistantAsteroid)other).Class);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(AsteroidNumber.PadLeft(6) + " " + AsteroidID.PadRight(13).Substring(0, 13) + "  on " + date);
			stringBuilder.Append(",  " + Class);
			return stringBuilder.ToString();
		}
	}
}
