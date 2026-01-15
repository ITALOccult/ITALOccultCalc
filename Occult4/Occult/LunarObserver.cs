using System;

namespace Occult
{
	internal class LunarObserver : IComparable
	{
		private string observer;

		private int count;

		internal string Observer
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

		internal int Count
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
			return observer.CompareTo(((LunarObserver)other).observer);
		}

		public override string ToString()
		{
			return observer.PadRight(21) + string.Format("{0,4:F0}", count);
		}
	}
}
