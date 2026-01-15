using System;
using System.Collections.Generic;

namespace Occult.Asteroid_Observations
{
	internal class EditLines : IComparable
	{
		private double eventJD;

		public int AstNum;

		public string AsteroidName = "";

		public string Date = "";

		public List<string> Edits = new List<string>();

		public double EventJD
		{
			get
			{
				return eventJD;
			}
			set
			{
				eventJD = value;
			}
		}

		public string EventID => Date + AstNum.ToString().PadLeft(7) + "  " + AsteroidName;

		public int CompareTo(object other)
		{
			return EventJD.CompareTo(((EditLines)other).EventJD);
		}
	}
}
