using System;

namespace Occult.Asteroid_Observations
{
	internal class AsteroidID : IComparable
	{
		private int number;

		private string iD;

		internal int Number
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

		internal string ID
		{
			get
			{
				return iD;
			}
			set
			{
				iD = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Number == ((AsteroidID)other).Number)
			{
				return ID.CompareTo(((AsteroidID)other).ID);
			}
			if (Number == 0)
			{
				return 1;
			}
			if (((AsteroidID)other).Number == 0)
			{
				return -1;
			}
			return Number.CompareTo(((AsteroidID)other).Number);
		}
	}
}
