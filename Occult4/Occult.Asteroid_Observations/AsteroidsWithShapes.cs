using System;

namespace Occult.Asteroid_Observations
{
	internal class AsteroidsWithShapes : IComparable
	{
		private int asteroidNumber;

		private int count = 1;

		private string asteroidID = "";

		public static int SortField;

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

		public string AsteroidID
		{
			get
			{
				return Utilities.HTML_DecodeString(asteroidID);
			}
			set
			{
				asteroidID = value;
			}
		}

		public int Count
		{
			get
			{
				return count;
			}
			set
			{
				count = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (AsteroidNumber.CompareTo(((AsteroidsWithShapes)other).AsteroidNumber) == 0)
				{
					return AsteroidID.CompareTo(((AsteroidsWithShapes)other).AsteroidID);
				}
				return AsteroidNumber.CompareTo(((AsteroidsWithShapes)other).AsteroidNumber);
			case 1:
				return AsteroidID.CompareTo(((AsteroidsWithShapes)other).AsteroidID);
			case 2:
				if (Count.CompareTo(((AsteroidsWithShapes)other).Count) == 0)
				{
					return AsteroidNumber.CompareTo(((AsteroidsWithShapes)other).AsteroidNumber);
				}
				return -Count.CompareTo(((AsteroidsWithShapes)other).Count);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			return AsteroidNumber.ToString().PadLeft(7) + " " + AsteroidID.PadRight(14) + Count.ToString().PadLeft(3);
		}
	}
}
