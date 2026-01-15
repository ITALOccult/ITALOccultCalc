using System;

namespace Occult.Lunar_Observations
{
	internal class OBSf : IComparable
	{
		private string obs;

		public string Obs
		{
			get
			{
				return obs;
			}
			set
			{
				obs = value;
			}
		}

		public int CompareTo(object other)
		{
			return Obs.Substring(8, 9).CompareTo(((OBSf)other).Obs.Substring(8, 9));
		}
	}
}
