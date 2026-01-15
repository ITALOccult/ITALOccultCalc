using System;

namespace Occult
{
	internal class HipStars : IComparable
	{
		private int hip;

		private ulong source_ID;

		private string catID;

		public int Hip
		{
			get
			{
				return hip;
			}
			set
			{
				hip = value;
			}
		}

		public ulong Source_ID
		{
			get
			{
				return source_ID;
			}
			set
			{
				source_ID = value;
			}
		}

		public string CatID
		{
			get
			{
				return catID;
			}
			set
			{
				catID = value;
			}
		}

		public int CompareTo(object other)
		{
			return Hip.CompareTo(((HipStars)other).Hip);
		}

		public override string ToString()
		{
			return Hip + "," + CatID;
		}
	}
}
