using System;

namespace Occult.Lunar_Observations
{
	internal class SiteList : IComparable
	{
		private string site;

		internal static int SortField;

		public string Site
		{
			get
			{
				return site;
			}
			set
			{
				site = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				return Site.Substring(0, 9).CompareTo(((SiteList)other).Site.Substring(0, 9));
			case 1:
				return Site.Substring(10).CompareTo(((SiteList)other).Site.Substring(10));
			case 2:
				return Site.Substring(24).CompareTo(((SiteList)other).Site.Substring(24));
			case 3:
				return Site.Substring(45).CompareTo(((SiteList)other).Site.Substring(45));
			case 4:
				if ((Site.Substring(91).Trim().Length < 1) & (((SiteList)other).Site.Substring(91).Trim().Length < 1))
				{
					return 0;
				}
				if ((Site.Substring(91).Trim().Length > 0) & (((SiteList)other).Site.Substring(91).Trim().Length < 1))
				{
					return -1;
				}
				if ((Site.Substring(91).Trim().Length < 1) & (((SiteList)other).Site.Substring(91).Trim().Length > 0))
				{
					return 1;
				}
				return Site.Substring(91).CompareTo(((SiteList)other).Site.Substring(91));
			default:
				if (((SiteList)other).Site.Substring(75, 5) == Site.Substring(75, 5))
				{
					return Site.Substring(0, 6).CompareTo(((SiteList)other).Site.Substring(0, 6));
				}
				return ((SiteList)other).Site.Substring(75, 5).CompareTo(Site.Substring(75, 5));
			}
		}
	}
}
