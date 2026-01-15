using System;

namespace Occult.Asteroid_Observations
{
	internal class NonStars : IComparable
	{
		private string star = "";

		private string line1 = "";

		private string line2 = "";

		private int cat;

		internal string Star
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

		internal string Line1
		{
			get
			{
				return line1;
			}
			set
			{
				line1 = value;
			}
		}

		internal string Line2
		{
			get
			{
				return line2;
			}
			set
			{
				line2 = value;
			}
		}

		internal int Cat
		{
			get
			{
				return cat;
			}
			set
			{
				cat = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Cat == ((NonStars)other).Cat)
			{
				return Star.CompareTo(((NonStars)other).Star);
			}
			return Cat.CompareTo(((NonStars)other).Cat);
		}
	}
}
