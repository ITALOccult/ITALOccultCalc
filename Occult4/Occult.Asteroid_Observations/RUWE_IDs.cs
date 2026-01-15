using System;

namespace Occult.Asteroid_Observations
{
	internal class RUWE_IDs : IComparable
	{
		private double ruwe;

		private string id;

		public double RUWE
		{
			get
			{
				return ruwe;
			}
			set
			{
				ruwe = value;
			}
		}

		internal string ID
		{
			get
			{
				return id;
			}
			set
			{
				id = value;
			}
		}

		public int CompareTo(object other)
		{
			return RUWE.CompareTo(((RUWE_IDs)other).RUWE);
		}

		public override string ToString()
		{
			return string.Format("{0,5:f2}  ", RUWE) + ID;
		}
	}
}
