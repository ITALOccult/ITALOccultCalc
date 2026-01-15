using System;

namespace Occult.Lunar_Observations
{
	internal class TELf : IComparable
	{
		private string tel;

		public string Tel
		{
			get
			{
				return tel;
			}
			set
			{
				tel = value;
			}
		}

		public int CompareTo(object other)
		{
			return Tel.Substring(8, 9).CompareTo(((TELf)other).Tel.Substring(8, 9));
		}
	}
}
