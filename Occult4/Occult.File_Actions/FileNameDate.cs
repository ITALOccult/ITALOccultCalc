using System;

namespace Occult.File_Actions
{
	internal class FileNameDate : IComparable
	{
		private string fname;

		private DateTime date;

		private long size;

		internal string FName
		{
			get
			{
				return fname;
			}
			set
			{
				fname = value;
			}
		}

		internal DateTime Date
		{
			get
			{
				return date;
			}
			set
			{
				date = value;
			}
		}

		internal long Size
		{
			get
			{
				return size;
			}
			set
			{
				size = value;
			}
		}

		public int CompareTo(object other)
		{
			return FName.CompareTo(((FileNameDate)other).FName);
		}
	}
}
