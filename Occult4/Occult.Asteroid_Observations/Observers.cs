using System;

namespace Occult.Asteroid_Observations
{
	internal class Observers : IComparable
	{
		private string observer = "";

		private int count;

		private int region;

		internal static int Sort;

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

		public int Region
		{
			get
			{
				return region;
			}
			set
			{
				region = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Sort == 0)
			{
				if (((Observers)other).Count == Count)
				{
					return Observer.Substring(3).CompareTo(((Observers)other).Observer.Substring(3));
				}
				return ((Observers)other).Count.CompareTo(Count);
			}
			if (Sort == 1)
			{
				return Observer.CompareTo(((Observers)other).Observer);
			}
			return Observer.Substring(3).CompareTo(((Observers)other).Observer.Substring(3));
		}
	}
}
