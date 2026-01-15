using System;

namespace Occult
{
	public class OccultException : Exception
	{
		public OccultException()
		{
		}

		public OccultException(string message, Exception innerException)
			: base(message, innerException)
		{
		}
	}
}
