using System;

namespace Occult
{
	internal class CompStars : IComparable
	{
		private string entry;

		private double mag;

		internal string Entry
		{
			get
			{
				return entry;
			}
			set
			{
				entry = value;
				int startIndex = value.IndexOf("mV=") + 3;
				double.TryParse(value.Substring(startIndex, 4), out mag);
			}
		}

		internal double Mag => mag;

		public int CompareTo(object other)
		{
			return Mag.CompareTo(((CompStars)other).Mag);
		}
	}
}
